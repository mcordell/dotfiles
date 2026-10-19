{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  # Both of upstream's "do this once" guards in the crowdsec preStart script are
  # broken. Patch the module source in place rather than vendoring a ~900-line
  # copy, so it keeps tracking nixpkgs.
  # Upstream: nixos/modules/services/security/crowdsec.nix
  crowdsecModule = "${inputs.nixpkgs}/nixos/modules/services/security/crowdsec.nix";
  upstreamSrc = builtins.readFile crowdsecModule;

  patches = [
    {
      name = "capi-register-check";
      # Stray `]` left over from an `[ ! -e ... ]` test:
      #   if ! grep -q password "$capiFile" ]; then
      # Bash reads the `]` as a second filename, so grep always exits 2, `!`
      # flips that to true, and `cscli capi register` re-runs on every start
      # even when valid credentials already exist.
      from = ''password "''${cfg.settings.capi.credentialsFile}" ]; then'';
      to = ''password "''${cfg.settings.capi.credentialsFile}"; then'';
    }
    {
      name = "console-enroll-condition";
      # Inverted test: enrolls only when the token file is *absent*, then cats
      # that same absent file -- so `cscli console enroll` never runs at all.
      # Enroll when the token exists and we have not already enrolled.
      from = ''if [ ! -e "''${cfg.settings.console.tokenFile}" ]; then'';
      to = ''if [ -e "''${cfg.settings.console.tokenFile}" ] && [ ! -e "''${stateDir}/.console-enrolled" ]; then'';
    }
    {
      name = "console-enroll-once";
      # Record success so enrollment is not re-sent on every start. The
      # `|| echo` also keeps a failure (app.crowdsec.net unreachable at boot,
      # bad key) from killing ExecStartPre under `set -e`: the list still exits
      # 0, and the absent sentinel means it retries on the next start.
      from = ''})" --name ''${cfg.name}'';
      to = ''})" --name ''${cfg.name} && ''${lib.getExe' pkgs.coreutils "touch"} "''${stateDir}/.console-enrolled" || echo "crowdsec: console enrollment failed, retrying on next start" >&2'';
    }
    {
      name = "etc-config-yaml";
      # The module keeps the generated config in the store and passes `-c` to
      # crowdsec and to its own cscli wrapper, but never creates
      # /etc/crowdsec/config.yaml -- cscli's default location. The firewall
      # bouncer's register unit invokes the *unwrapped* cscli with no `-c`, so
      # it dies with:
      #   Error: while reading yaml file: open /etc/crowdsec/config.yaml:
      #   no such file or directory
      # Exposing the config at the conventional path fixes that unit and also
      # makes an interactive bare `cscli` work.
      from = "services.crowdsec.settings.general = {";
      to = ''
        environment.etc."crowdsec/config.yaml".source = configFile;

        services.crowdsec.settings.general = {'';
    }
  ];

  patchedSrc = lib.foldl' (
    src: p: builtins.replaceStrings [ p.from ] [ p.to ] src
  ) upstreamSrc patches;

  # `cscli console enroll` has to *write* the console config, but upstream
  # defaults console_path to an immutable /nix/store file:
  #   failed writing console config: ... open /nix/store/...-console.yaml:
  #   read-only file system
  # Keep it in writable state instead, seeded from the module's own option so
  # `crowdsec -t` config validation still passes on a fresh machine.
  consoleStateFile = "/var/lib/crowdsec/state/console.yaml";
  consoleSeed =
    (pkgs.formats.yaml { }).generate "console.yaml"
      config.services.crowdsec.settings.console.configuration;
in
{
  disabledModules = [ "services/security/crowdsec.nix" ];
  imports = [ (builtins.toFile "crowdsec-prestart-fixes.nix" patchedSrc) ];

  # Fail the build if a nixpkgs bump fixes or reworks any patched line, so these
  # workarounds can't silently rot into no-ops.
  assertions = map (p: {
    assertion = lib.hasInfix p.from upstreamSrc;
    message = ''
      The CrowdSec preStart patch "${p.name}" in nix/hosts/cattle/crowdsec.nix
      no longer applies -- upstream has likely fixed or reworked it. Re-check
      nixos/modules/services/security/crowdsec.nix and drop the stale entry.
    '';
  }) patches;

  services.crowdsec = {
    enable = true;

    settings = {
      general.api.server.enable = true;
      # Overrides upstream's read-only store path (see consoleStateFile above).
      general.api.server.console_path = consoleStateFile;
      lapi.credentialsFile = "/var/lib/crowdsec/lapi-credentials.yaml";
      # Setting capi.credentialsFile prevents null coercion in the upstream module
      # (online_client.credentials_path defaults to null without this).
      # The preStart script auto-registers with CAPI when this path is set.
      capi.credentialsFile = "/var/lib/crowdsec/state/online_api_credentials.yaml";
      console.tokenFile = config.sops.secrets."crowdsec/console_enrollment_key".path;
    };

    localConfig.acquisitions = [
      {
        filenames = [ "/var/lib/pangolin/config/traefik/logs/access.log" ];
        labels.type = "traefik";
      }
      {
        source = "journalctl";
        journalctl_filter = [ "_SYSTEMD_UNIT=sshd.service" ];
        labels.type = "syslog";
      }
    ];

    hub.parsers = [
      "crowdsecurity/syslog-logs"
      # Supplies the GeoLite2-City/ASN mmdb data files into data_dir
      # (/var/lib/crowdsec/state). Without it crowdsec logs
      #   unable to initialize GeoIP: open .../GeoLite2-City.mmdb: no such file
      # and alerts carry no country/ASN enrichment. None of the collections
      # above pull it in.
      "crowdsecurity/geoip-enrich"
    ];

    hub.collections = [
      "crowdsecurity/traefik"
      "crowdsecurity/http-cve"
      "crowdsecurity/base-http-scenarios"
      "crowdsecurity/sshd"
    ];
  };

  # Ensure data/log directories exist and are traversable by crowdsec
  systemd.tmpfiles.rules = [
    # 0750: this dir holds the lapi/capi credential files
    "d /var/lib/crowdsec 0750 crowdsec crowdsec -"
    # state/ is also created by the module's own 10-crowdsec.conf, but that
    # file is processed after 00-nixos.conf, so create it here for the `C` rule.
    "d /var/lib/crowdsec/state 0750 crowdsec crowdsec -"
    # `C` copies the seed only when the file does not already exist, so writes
    # from `cscli console enroll` survive.
    "C ${consoleStateFile} 0600 crowdsec crowdsec - ${consoleSeed}"
    "d /var/lib/pangolin 0755 root root -"
    "d /var/lib/pangolin/config 0755 root root -"
    "d /var/lib/pangolin/config/traefik 0755 root root -"
    "d /var/lib/pangolin/config/traefik/logs 0755 root root -"
  ];

  systemd.services.crowdsec.serviceConfig = {
    # Allow CrowdSec to read Docker-mounted Traefik logs
    ReadOnlyPaths = [
      "/var/lib/pangolin/config/traefik/logs"
    ];

    # Upstream gives crowdsec no StateDirectory -- it reaches /var/lib/crowdsec
    # only through ReadWritePaths, and its preStart `mkdir -p` cannot create
    # that directory because ProtectSystem=strict leaves /var/lib read-only. So
    # upstream depends on the *bouncer's* register unit to create it via
    # StateDirectory=crowdsec, despite crowdsec being ordered before that unit
    # (Before=crowdsec-firewall-bouncer-register.service). Let crowdsec own its
    # own state directory instead.
    StateDirectory = "crowdsec";
    StateDirectoryMode = "0750";
    # StateDirectory + DynamicUser would relocate the directory into root-only
    # /var/lib/private -- the original failure. User=crowdsec is a static system
    # user declared by the module, so DynamicUser never allocated anything here
    # anyway, and every protection it implies (PrivateTmp, RemoveIPC,
    # NoNewPrivileges, ...) is already set explicitly by the module.
    DynamicUser = lib.mkForce false;
  };

  # CrowdSec firewall bouncer for IP-level blocking via nftables
  services.crowdsec-firewall-bouncer = {
    enable = true;
    settings = {
      mode = "nftables";
    };
  };

  # Enable nftables (required by the firewall bouncer)
  networking.nftables.enable = true;
  # Workaround for #476253: fix bouncer systemd ordering
  systemd.services.crowdsec-firewall-bouncer = {
    after = [
      "nftables.service"
      "crowdsec.service"
    ];
    requires = [ "crowdsec.service" ];
  };

  # The register unit declares `StateDirectory = "... crowdsec"` with
  # DynamicUser, which makes systemd relocate /var/lib/crowdsec into
  # root-only (0700) /var/lib/private and leave a symlink behind. crowdsec
  # itself has no StateDirectory -- it reaches that path via ReadWritePaths --
  # so it then cannot traverse the parent and dies on
  #   mkdir: cannot create directory '/var/lib/crowdsec': Permission denied
  # Running the register unit as the static crowdsec user keeps the directory
  # public; systemd migrates it back out of /var/lib/private on next start.
  systemd.services.crowdsec-firewall-bouncer-register.serviceConfig = {
    DynamicUser = lib.mkForce false;
    # Drop "crowdsec" from the list: this unit no longer owns a directory
    # belonging to a service that starts before it (see crowdsec.service above).
    # It still reaches /var/lib/crowdsec/state/crowdsec.db to run
    # `cscli bouncers add` -- it has ProtectSystem=no and runs as User=crowdsec,
    # which owns that tree, so it needs no ReadWritePaths grant.
    StateDirectory = lib.mkForce "crowdsec-firewall-bouncer-register";
    # StateDirectory would otherwise reset the dir to 0755 on every start.
    StateDirectoryMode = "0750";
  };
}
