{ config, pkgs, lib, ... }:
{
  services.crowdsec = {
    enable = true;
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
      mode = "nftables";
    };
  };

  # Workaround for #476253: fix bouncer systemd ordering
  systemd.services.crowdsec-firewall-bouncer = {
    after = [ "nftables.service" "crowdsec.service" ];
    requires = [ "crowdsec.service" ];
  };
}
