{
  config,
  lib,
  ...
}:
let
  # Where this box has kept CrowdSec state since before the nixpkgs 26.11 module
  # rewrite (nixpkgs PR #446307). The rewritten module defaults data_dir to
  # /var/lib/crowdsec/data; letting that default win would start CrowdSec on an
  # empty database and lose the machine/bouncer registrations, CAPI credentials,
  # console enrollment and GeoIP files. hub_dir, index_path, db_path and
  # console_path all derive from data_dir, so pinning it keeps every one of them
  # where it was (state/hub, state/crowdsec.db, state/console.yaml).
  stateDir = "/var/lib/crowdsec/state";
  lapiCredentials = "/var/lib/crowdsec/lapi-credentials.yaml";

  # Table/chain/set names for the hand-written nftables rulesets below.
  bouncerSettings = config.services.crowdsec-firewall-bouncer.settings;
in
{
  # Dropped with the rewrite, because upstream now covers them:
  # - The capi-register and console-enroll preStart patches. Setup moved to a
  #   separate crowdsec-setup.service; its CAPI check (`[ ! -s ... ]`) and its
  #   enrollment test are both correct.
  # - The once-only enrollment sentinel. crowdsec-setup runs
  #   `cscli console enroll` on every start, but cscli (1.8.1) treats the
  #   API's 200 "already enrolled" reply as success and changes nothing. A
  #   failure (offline at boot) now fails only crowdsec-setup; crowdsec merely
  #   Wants it, so the engine still starts.
  # - /etc/crowdsec/config.yaml. Upstream links the packaged config there and
  #   puts the generated overrides in config.yaml.local, which cscli merges, so
  #   a bare `cscli` (the bouncer register unit, interactive use) works.
  # - The StateDirectory/DynamicUser overrides on crowdsec and the register
  #   unit. Neither unit declares a StateDirectory any more, so systemd never
  #   relocates /var/lib/crowdsec into /var/lib/private. DynamicUser with the
  #   static `crowdsec` user just reuses that user.
  # - The bouncer ordering workaround (#476253). The bouncer is now ordered
  #   after nftables, crowdsec and the register unit by upstream.

  services.crowdsec = {
    enable = true;

    # Docker-mounted Traefik logs, read by the first acquisition below.
    readOnlyPaths = [ "/var/lib/pangolin/config/traefik/logs" ];

    settings = {
      config = {
        config_paths.data_dir = stateDir;
        api.client.credentials_path = lapiCredentials;
        # Non-null also makes crowdsec-setup register with CAPI when the file
        # is missing or empty.
        api.server.online_client.credentials_path = "${stateDir}/online_api_credentials.yaml";
      };

      # Loaded with LoadCredential (read by PID 1 as root), so the sops secret
      # no longer needs to be readable by the crowdsec user.
      console.enrollKeyFile = config.sops.secrets."crowdsec/console_enrollment_key".path;

      # console.configuration is left at upstream's default (every share_*
      # option false), which matches the box's console.yaml as of 2026-11-17.
      # Upstream rewrites console_path from that option on every tmpfiles run
      # (an `f+` rule), so `cscli console enable ...` does not survive a reboot
      # or a switch. To change sharing, set console.configuration here.

      acquisitions = [
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
    };

    hub.parsers = [
      "crowdsecurity/syslog-logs"
      # Supplies the GeoLite2-City/ASN mmdb data files into data_dir
      # (/var/lib/crowdsec/state). Without it crowdsec logs
      #   unable to initialize GeoIP: open .../GeoLite2-City.mmdb: no such file
      # and alerts carry no country/ASN enrichment. None of the collections
      # below pull it in.
      "crowdsecurity/geoip-enrich"
    ];

    hub.collections = [
      "crowdsecurity/traefik"
      "crowdsecurity/http-cve"
      "crowdsecurity/base-http-scenarios"
      "crowdsecurity/sshd"
    ];
  };

  # lapi-credentials.yaml sits directly in /var/lib/crowdsec, outside the
  # module's ReadWritePaths (which cover data_dir, i.e. state/). Reading it works
  # as is; this grant lets crowdsec-setup's `cscli machines add --file` create it
  # on a fresh machine. Kept at the old path rather than moved into state/ so
  # the existing file is used without a manual migration.
  systemd.services.crowdsec-setup.serviceConfig.ReadWritePaths = [ "/var/lib/crowdsec" ];

  systemd.tmpfiles.rules = [
    # Upstream only creates the subdirectories it uses (state/ and below);
    # tmpfiles would otherwise create this parent as root 0755. 0750 because it
    # holds the lapi credentials file.
    "d /var/lib/crowdsec 0750 crowdsec crowdsec -"
    # readOnlyPaths must exist before crowdsec starts, or the unit fails to
    # set up its mount namespace.
    "d /var/lib/pangolin 0755 root root -"
    "d /var/lib/pangolin/config 0755 root root -"
    "d /var/lib/pangolin/config/traefik 0755 root root -"
    "d /var/lib/pangolin/config/traefik/logs 0755 root root -"
  ];

  # CrowdSec firewall bouncer for IP-level blocking via nftables
  services.crowdsec-firewall-bouncer = {
    enable = true;
    settings = {
      mode = "nftables";
    };
    # The module's own ruleset puts the drop rule in `hook input`, which never
    # sees traffic bound for a Docker container: Docker DNATs published ports in
    # nat/PREROUTING (priority -100), after which the packet is routed to the
    # container and traverses `hook forward` instead. Pangolin publishes 80/443
    # on gerbil and Traefik shares its netns, so every HTTP ban landed in a set
    # the traffic never passed through. Declare the rulesets below instead,
    # hooked ahead of DNAT. (Upstream documents this option for exactly this:
    # "You may disable this option to manually configure it.")
    createRulesets = false;
  };

  # Enable nftables (required by the firewall bouncer)
  networking.nftables.enable = true;

  # Replaces the rulesets the module would have generated. Each set must live in
  # the same table as the rule reading it -- nftables sets are table-scoped --
  # and must keep the names the bouncer is configured with, because in set-only
  # mode the bouncer only adds and expires elements of an existing set. Names
  # are taken from the bouncer settings rather than hardcoded so the two cannot
  # drift apart.
  networking.nftables.tables = {
    ${bouncerSettings.nftables.ipv4.table} = {
      family = "ip";
      content = ''
        set ${bouncerSettings.blacklists_ipv4} {
          type ipv4_addr
          flags timeout
        }

        chain ${bouncerSettings.nftables.ipv4.chain} {
          # priority raw (-300) runs ahead of nat prerouting (-100), so this
          # covers host-bound and container-bound traffic alike. `hook input`
          # (the upstream default) only ever saw the former.
          type filter hook prerouting priority raw; policy accept;
          # This chain now also sees forwarded and inter-container packets, so
          # never let a decision on a private address break the Docker network
          # or LAN access. CrowdSec should never ban these -- Traefik logs the
          # real public client IP -- but the blast radius if it did is too big.
          ip saddr {
            127.0.0.0/8,
            10.0.0.0/8,
            172.16.0.0/12,
            192.168.0.0/16,
          } return
          ip saddr @${bouncerSettings.blacklists_ipv4} counter drop
        }
      '';
    };
    ${bouncerSettings.nftables.ipv6.table} = {
      family = "ip6";
      content = ''
        set ${bouncerSettings.blacklists_ipv6} {
          type ipv6_addr
          flags timeout
        }

        chain ${bouncerSettings.nftables.ipv6.chain} {
          type filter hook prerouting priority raw; policy accept;
          # Loopback, unique-local and link-local -- see the ipv4 chain above.
          ip6 saddr {
            ::1/128,
            fc00::/7,
            fe80::/10,
          } return
          ip6 saddr @${bouncerSettings.blacklists_ipv6} counter drop
        }
      '';
    };
  };
}
