{ config, ... }:
{
  sops = {
    defaultSopsFile = ./secrets.yaml;
    age.keyFile = "/var/lib/sops-nix/key.txt";

    secrets = {
      "pangolin/server_secret" = { };
      "pangolin/setup_token" = { };
      # After first boot, generate with: sudo cscli bouncers add traefik-bouncer
      # Then add to secrets.yaml and re-encrypt
      "crowdsec/bouncer_api_key" = { };
      # crowdsec.service runs ExecStartPre as User=crowdsec, which cats this
      # key to run `cscli console enroll`. sops-nix defaults to root:root 0400,
      # which that user cannot read.
      "crowdsec/console_enrollment_key" = {
        owner = config.services.crowdsec.user;
        mode = "0400";
      };
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
