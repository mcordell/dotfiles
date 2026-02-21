{ config, ... }:
{
  sops = {
    defaultSopsFile = ./secrets.yaml;
    age.keyFile = "/var/lib/sops-nix/key.txt";

    secrets = {
      "pangolin/server_secret" = { };
      "pangolin/setup_token" = { };
      "pangolin/cf_dns_api_token" = { };
      # After first boot, generate with: sudo cscli bouncers add traefik-bouncer
      # Then add to secrets.yaml and re-encrypt
      "crowdsec/bouncer_api_key" = {};
      "crowdsec/console_enrollment_key" = {};
    };
  };

  # Pangolin sops templates are defined in pangolin.nix
}
