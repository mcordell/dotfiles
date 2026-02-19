{ config, pkgs, lib, ... }:
{
  services.crowdsec = {
    enable = true;

    settings= {
        general.api.server.enable = true;
	lapi.credentialsFile = "/var/lib/crowdsec/lapi-credentials.yaml";
    };

    localConfig.acquisitions = [
      {
        filenames = [ "/var/log/traefik/access.log" ];
        labels.type = "traefik";
      }
    ];

    hub.collections = [
      "crowdsecurity/traefik"
      "crowdsecurity/http-cve"
      "crowdsecurity/base-http-scenarios"
    ];
  };

  # Ensure data/log directories exist
  systemd.tmpfiles.rules = [
    "d /var/lib/crowdsec 0755 crowdsec crowdsec -"
    "d /var/log/traefik 0755 root root -"
  ];

  # CrowdSec firewall bouncer for IP-level blocking via nftables
  # registerBouncer.enable defaults to true when services.crowdsec is enabled,
  # so no manual API key management is needed.
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
