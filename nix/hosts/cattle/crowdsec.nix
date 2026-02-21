{ config, pkgs, lib, ... }:
{
  services.crowdsec = {
    enable = true;

    settings = {
      general.api.server.enable = true;
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
    "d /var/lib/crowdsec 0755 crowdsec crowdsec -"
    "d /var/lib/pangolin 0755 root root -"
    "d /var/lib/pangolin/config 0755 root root -"
    "d /var/lib/pangolin/config/traefik 0755 root root -"
    "d /var/lib/pangolin/config/traefik/logs 0755 root root -"
  ];

  # Allow CrowdSec to read Docker-mounted Traefik logs
  systemd.services.crowdsec.serviceConfig.ReadOnlyPaths = [
    "/var/lib/pangolin/config/traefik/logs"
  ];

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
    after = [ "nftables.service" "crowdsec.service" ];
    requires = [ "crowdsec.service" ];
  };
}
