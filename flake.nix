{
  description = "Unified NixOS + nix-darwin + Home Manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # sherpa machine wiring (configs + launchd agents). Private Gitea, so the
    # fetch is SSH and costs a smartcard touch. While iterating on the module,
    # skip that with:
    #   darwin-rebuild switch --flake . \
    #     --override-input pimalaya path:/Users/michael/Code/rust/pimalaya
    pimalaya = {
      url = "git+ssh://git@git.westeros.lan/michael/pimalaya.git";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      nix-darwin,
      ...
    }@inputs:
    let
      lib = nixpkgs.lib;

      # System type helpers
      isLinux = system: lib.hasSuffix "linux" system;
      isDarwin = system: lib.hasSuffix "darwin" system;
      getHomeDirectory = system: user: if isDarwin system then "/Users/${user}" else "/home/${user}";

      # Path helpers
      hostPath = hostname: file: lib.path.append ./nix/hosts "${hostname}/${file}";

      # Host validation
      validateHost =
        hostname: cfg:
        assert lib.assertMsg (cfg ? system) "Host ${hostname} must have 'system'";
        assert lib.assertMsg (cfg ? user) "Host ${hostname} must have 'user'";
        assert lib.assertMsg (cfg ? type) "Host ${hostname} must have 'type'";
        assert lib.assertMsg (lib.elem cfg.type [
          "nixos"   # Full NixOS system
          "darwin"  # macOS with nix-darwin
          "linux"   # Regular Linux with standalone Home Manager (Debian, Ubuntu, etc.)
        ]) "Host ${hostname} has invalid type: ${cfg.type}";
        cfg;

      # ---- central "inventory" of machines/users ----
      hosts = {
        # NixOS systems (full system management)
        nixos = {
          system = "x86_64-linux";
          user = "michael";
          type = "nixos";
        };

        cattle = {
          system = "x86_64-linux";
          user = "michael";
          type = "nixos";
        };

        # macOS systems (nix-darwin)
        "Michaels-MacBook-Pro" = {
          system = "aarch64-darwin";
          user = "michael";
          type = "darwin";
        };

        "Michaels-MacBook-Air" = {
          system = "aarch64-darwin";
          user = "michael";
          type = "darwin";
        };

        # Non-NixOS Linux systems (standalone Home Manager only)
        # Examples for Debian/Ubuntu homelab servers:
        charlie = {
          system = "x86_64-linux";
          user = "michael";
          type = "linux";
        };

        shadow = {
          system = "x86_64-linux";
          user = "michael";
          type = "linux";
        };

        stella = {
          system = "x86_64-linux";
          user = "michael";
          type = "linux";
        };

        # Quoted and hyphenated because the attribute name has to match the
        # machine's real hostname, which is `big-box` — the Ansible inventory
        # calls the same box `bigbox`. provision-lix.yml derives the flake
        # target from ansible_facts['hostname'], so `bigbox` would not resolve.
        "big-box" = {
          system = "x86_64-linux";
          user = "michael";
          type = "linux";
        };
      };

      # Common Home Manager module set
      hmModulesFor =
        { system, hostname }:
        [
          ./nix/home/default.nix
        ]
        ++ lib.optionals (isLinux system) [
          ./nix/home/linux.nix
        ]
        ++ lib.optionals (isDarwin system) [
          ./nix/home/darwin.nix
        ]
        ++ lib.optionals (builtins.pathExists (hostPath hostname "nix/home/home.nix")) [
          (hostPath hostname "nix/home/home.nix")
        ];

      # Shared Home Manager configuration
      mkHomeManagerConfig =
        {
          system,
          user,
          hostname,
        }:
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.${user} = {
            home.username = user;
            home.homeDirectory = lib.mkForce (getHomeDirectory system user);
            imports = hmModulesFor { inherit system hostname; };
          };
        };

      # NixOS system constructor
      mkNixos =
        hostname: cfg:
        let
          validated = validateHost hostname cfg;
        in
        lib.nixosSystem {
          system = validated.system;
          specialArgs = {
            inherit inputs hostname;
            user = validated.user;
          };
          modules = [
            { nixpkgs.overlays = nixpkgsOverlays; }
            (hostPath hostname "configuration.nix")

            # Encrypted secrets management
            inputs.sops-nix.nixosModules.sops

            # Home Manager integrated into NixOS
            home-manager.nixosModules.home-manager
            (mkHomeManagerConfig {
              system = validated.system;
              user = validated.user;
              inherit hostname;
            })
          ];
        };

      # nix-darwin system constructor
      mkDarwin =
        hostname: cfg:
        let
          validated = validateHost hostname cfg;
        in
        nix-darwin.lib.darwinSystem {
          specialArgs = {
            inherit inputs hostname;
            user = validated.user;
          };
          modules = [
            {
              nixpkgs.overlays = nixpkgsOverlays;
              nixpkgs.config.allowUnfreePredicate =
                pkg:
                builtins.elem (lib.getName pkg) [
                  "claude-code"
                ];
            }
            ./nix/darwin-configuration.nix
          ]
          ++ lib.optionals (builtins.pathExists (hostPath hostname "darwin-configuration.nix")) [
            (hostPath hostname "darwin-configuration.nix")
          ]
          ++ [

            # Home Manager integrated into nix-darwin
            home-manager.darwinModules.home-manager
            (mkHomeManagerConfig {
              system = validated.system;
              user = validated.user;
              inherit hostname;
            })
          ];
        };

      # Optional: standalone HM configs (useful for non-NixOS Linux boxes
      # where you don't want full NixOS rebuilds, or for experimenting)
      mkHome =
        name: cfg:
        let
          validated = validateHost name cfg;
        in
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = validated.system;
            overlays = nixpkgsOverlays;
            config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
              "claude-code"
            ];
          };
          modules =
            (hmModulesFor {
              system = validated.system;
              hostname = name;
            })
            ++ [
              {
                home.username = validated.user;
                home.homeDirectory = getHomeDirectory validated.system validated.user;
              }
            ];
        };

      # Helper to build configs by type
      mkConfigsByType =
        type: mkConfig:
        lib.mapAttrs (hostname: cfg: mkConfig hostname cfg) (
          lib.filterAttrs (_: cfg: cfg.type == type) hosts
        );

      # Empty on purpose. Two `doCheck = false` overrides lived here — nushell
      # 0.112.1 (flaky env_shlvl_in_exec_repl) and mise 2026.6.11 (OCI layer test
      # needs setuid bits the sandbox drops). Both upstream failures are gone as
      # of nushell 0.114.1 and mise 2026.7.17, which now substitute from
      # cache.nixos.org.
      #
      # Removing them is not just tidying. Any override changes the derivation
      # hash, so the binary cache is forfeited and the package is built locally —
      # and mise does not survive that: it lists `cmake`, `git` and `cacert` under
      # `nativeCheckInputs`, so `doCheck = false` strips them, and its
      # `libz-ng-sys` build script then dies with "is `cmake` not installed?".
      # Upstream only builds because running the tests happens to drag cmake in.
      #
      # So before adding an override here, check whether the failure it works
      # around is still real, and whether disabling checks removes something the
      # *build* silently depends on.
      nixpkgsOverlays = [ ];
    in
    {
      # ---- System-level configs ----
      nixosConfigurations = mkConfigsByType "nixos" mkNixos;

      darwinConfigurations = mkConfigsByType "darwin" mkDarwin;

      # ---- Standalone Home Manager configs (for non-NixOS Linux systems) ----
      homeConfigurations = mkConfigsByType "linux" mkHome;

      # ---- Formatter ----
      formatter = lib.genAttrs [ "x86_64-linux" "aarch64-darwin" ] (
        system: nixpkgs.legacyPackages.${system}.nixfmt-tree
      );
    };
}
