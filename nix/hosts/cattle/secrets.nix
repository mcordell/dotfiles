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
      # "crowdsec/bouncer_api_key" = {};
    };
  };

  # Pangolin environment file assembled from sops secrets
  sops.templates."pangolin.env" = {
    content = ''
      SERVER_SECRET=${config.sops.placeholder."pangolin/server_secret"}
      CF_DNS_API_TOKEN=${config.sops.placeholder."pangolin/cf_dns_api_token"}
    '';
  };
}
