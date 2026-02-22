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
      "crowdsec/bouncer_api_key" = { };
      "crowdsec/console_enrollment_key" = { };
      "real_estate/db_user" = { };
      "real_estate/db_password" = { };
      "real_estate/db_name" = { };
      "real_estate/secret_key_base" = { };
      "real_estate/mailgun_key" = { };
      "real_estate/phx_host" = { };
      "real_estate/aws_access_key" = { };
      "real_estate/aws_secret_access_key" = { };
      "real_estate/s3_file_upload_bucket" = { };
    };
  };

  # Pangolin sops templates are defined in pangolin.nix
}
