{ config, pkgs, ... }:
let
  dataDir = "/mnt/docker-data/data";
  configDir = "/var/lib/real-estate";

  composeFile = pkgs.writeText "docker-compose.yml" ''
    name: real-estate
    services:
      db:
        image: postgres:17.4-alpine
        container_name: real-estate-db
        restart: unless-stopped
        env_file:
          - ${configDir}/db.env
        volumes:
          - ${dataDir}/postgres:/var/lib/postgresql/data
        networks:
          - backend

      web:
        image: truenas:30095/real-estate:0.0.32
        container_name: real-estate-web
        restart: unless-stopped
        expose:
          - "4000"
        env_file:
          - ${configDir}/web.env
        working_dir: /app
        command: /app/bin/real_estate start
        depends_on:
          - db
        networks:
          pangolin:
          backend:

    networks:
      pangolin:
        external: true
      backend:
        driver: bridge
        internal: true
  '';

in
{
  sops.templates."real-estate-db.env" = {
    content = ''
      POSTGRES_USER=${config.sops.placeholder."real_estate/db_user"}
      POSTGRES_PASSWORD=${config.sops.placeholder."real_estate/db_password"}
      POSTGRES_DB=real_estate_web_prod
      POSTGRES_HOST=db
    '';
  };

  sops.templates."real-estate-web.env" = {
    content = ''
      HTTP_PORT=4000
      MIX_ENV=prod
      APP_PORT=4000
      DB_HOST=db
      DB_USER=${config.sops.placeholder."real_estate/db_user"}
      DB_PASSWORD=${config.sops.placeholder."real_estate/db_password"}
      APP_DATABASE=${config.sops.placeholder."real_estate/db_name"}
      SECRET_KEY_BASE=${config.sops.placeholder."real_estate/secret_key_base"}
      APP_HOSTNAME=${config.sops.placeholder."real_estate/phx_host"}
      MAILGUN_KEY=${config.sops.placeholder."real_estate/mailgun_key"}
      DATABASE_URL=ecto://${config.sops.placeholder."real_estate/db_user"}:${
        config.sops.placeholder."real_estate/db_password"
      }@db:5432/${config.sops.placeholder."real_estate/db_name"}
      PHX_HOST=${config.sops.placeholder."real_estate/phx_host"}
      AWS_ACCESS_KEY_ID=${config.sops.placeholder."real_estate/aws_access_key"}
      AWS_SECRET_ACCESS_KEY=${config.sops.placeholder."real_estate/aws_secret_access_key"}
      AWS_REGION=us-east-2
      S3_FILE_UPLOAD_BUCKET=${config.sops.placeholder."real_estate/s3_file_upload_bucket"}
    '';
  };

  systemd.tmpfiles.rules = [
    "d ${configDir} 0755 root root -"
  ];

  systemd.services.real-estate = {
    description = "Real Estate Docker Compose Stack";
    after = [
      "docker.service"
      "network-online.target"
      "sops-nix.service"
    ];
    wants = [ "network-online.target" ];
    requires = [ "docker.service" ];
    wantedBy = [ "multi-user.target" ];

    path = [ pkgs.docker ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    preStart = ''
      cp -f ${config.sops.templates."real-estate-db.env".path} ${configDir}/db.env
      cp -f ${config.sops.templates."real-estate-web.env".path} ${configDir}/web.env
    '';

    script = ''
      docker compose -f ${composeFile} --project-directory ${configDir} up -d --wait
    '';

    postStop = ''
      docker compose -f ${composeFile} --project-directory ${configDir} down
    '';
  };
}
