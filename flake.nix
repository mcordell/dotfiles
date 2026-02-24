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
    in
    {
      # ---- System-level configs ----
      nixosConfigurations = mkConfigsByType "nixos" mkNixos;

      darwinConfigurations = mkConfigsByType "darwin" mkDarwin;

      # ---- Standalone Home Manager configs (for non-NixOS Linux systems) ----
      homeConfigurations = mkConfigsByType "linux" mkHome;

      # ---- Formatter ----
      formatter = lib.genAttrs [ "x86_64-linux" "aarch64-darwin" ] (
        system: nixpkgs.legacyPackages.${system}.nixfmt
      );
    };
}
