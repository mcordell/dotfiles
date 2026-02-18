{ config, pkgs, lib, ... }:
{
  services.crowdsec = {
    enable = true;

    # Workaround for #445342: explicitly enable local API
    settings = {
      api.server = {
        enable = true;
        listen_uri = "127.0.0.1:8080";
      };
    };
  };

  # Ensure data/log directories exist
  systemd.tmpfiles.rules = [
    "d /var/lib/crowdsec 0755 crowdsec crowdsec -"
    "d /var/log/traefik 0755 root root -"
  ];

  # CrowdSec firewall bouncer for IP-level blocking via nftables
  services.crowdsec-firewall-bouncer = {
    enable = true;
    settings = {
      api_url = "http://127.0.0.1:8080";
      api_key = ""; # placeholder — set after first boot via cscli
      mode = "nftables";
    };
  };

  # Workaround for #476253: fix bouncer systemd ordering
  systemd.services.crowdsec-firewall-bouncer = {
    after = [ "nftables.service" "crowdsec.service" ];
    requires = [ "crowdsec.service" ];
  };
}
