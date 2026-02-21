{ config, pkgs, ... }:
let
  dataDir = "/var/lib/pangolin";
  configDir = "${dataDir}/config";

  composeFile = pkgs.writeText "docker-compose.yml" ''
    name: pangolin
    services:
      pangolin:
        image: docker.io/fosrl/pangolin:latest
        container_name: pangolin
        restart: unless-stopped
        volumes:
          - ${configDir}:/app/config
        healthcheck:
          test: ["CMD", "curl", "-f", "http://localhost:3001/api/v1/"]
          interval: "10s"
          timeout: "10s"
          retries: 15

      gerbil:
        image: docker.io/fosrl/gerbil:latest
        container_name: gerbil
        restart: unless-stopped
        depends_on:
          pangolin:
            condition: service_healthy
        command:
          - --reachableAt=http://gerbil:3004
          - --generateAndSaveKeyTo=/var/config/key
          - --remoteConfig=http://pangolin:3001/api/v1/
        volumes:
          - ${configDir}:/var/config
        cap_add:
          - NET_ADMIN
          - SYS_MODULE
        ports:
          - "51820:51820/udp"
          - "21820:21820/udp"
          - "443:443"
          - "80:80"

      traefik:
        image: docker.io/traefik:v3.6
        container_name: traefik
        restart: unless-stopped
        network_mode: service:gerbil
        depends_on:
          pangolin:
            condition: service_healthy
        command:
          - --configFile=/etc/traefik/traefik_config.yml
        env_file:
          - ${configDir}/traefik.env
        volumes:
          - ${configDir}/traefik:/etc/traefik:ro
          - ${configDir}/letsencrypt:/letsencrypt
          - ${configDir}/traefik/logs:/var/log/traefik

    networks:
      default:
        driver: bridge
        name: pangolin
  '';

  traefikStaticConfig = pkgs.writeText "traefik_config.yml" ''
    api:
      insecure: true
      dashboard: true

    providers:
      http:
        endpoint: "http://pangolin:3001/api/v1/traefik-config"
        pollInterval: "5s"
      file:
        filename: "/etc/traefik/dynamic_config.yml"

    experimental:
      plugins:
        badger:
          moduleName: "github.com/fosrl/badger"
          version: "v1.3.1"
        geoblock:
          moduleName: "github.com/david-garcia-garcia/traefik-geoblock"
          version: "v1.1.1"

    log:
      level: "INFO"

    accessLog:
      filePath: "/var/log/traefik/access.log"
      format: "json"

    certificatesResolvers:
      letsencrypt:
        acme:
          dnsChallenge:
            provider: cloudflare
          email: "surpher@gmail.com"
          storage: "/letsencrypt/acme.json"
          caServer: "https://acme-v02.api.letsencrypt.org/directory"

    entryPoints:
      web:
        address: ":80"
      websecure:
        address: ":443"
        transport:
          respondingTimeouts:
            readTimeout: "30m"
        http:
          middlewares:
            - geoblock@file
            - security-headers@file
          tls:
            certResolver: "letsencrypt"

    serversTransport:
      insecureSkipVerify: true

    ping:
      entryPoint: "web"
  '';

  traefikDynamicConfig = pkgs.writeText "dynamic_config.yml" ''
    http:
      middlewares:
        badger:
          plugin:
            badger:
              disableForwardAuth: true
        geoblock:
          plugin:
            geoblock:
              enabled: true
              defaultAllow: false
              allowPrivate: true
              disallowedStatusCode: 403
              databaseFilePath: "/plugins-storage/"
              allowedCountries:
                - US
                - CA
              allowedIPBlocks:
                - "192.168.0.0/16"
                - "10.0.0.0/8"
              databaseAutoUpdate: true
              databaseAutoUpdateDir: "/plugins-storage/geoblock-db"
        security-headers:
          headers:
            contentTypeNosniff: true
            customFrameOptionsValue: SAMEORIGIN
            customResponseHeaders:
              Server: ""
              X-Powered-By: ""
            forceSTSHeader: true
            hostsProxyHeaders:
              - X-Forwarded-Host
            referrerPolicy: strict-origin-when-cross-origin
            sslProxyHeaders:
              X-Forwarded-Proto: https
            stsIncludeSubdomains: true
            stsPreload: true
            stsSeconds: 63072000
        redirect-to-https:
          redirectScheme:
            scheme: https

      routers:
        main-app-router-redirect:
          rule: "Host(`pangolin.mcordell.dev`)"
          service: next-service
          entryPoints:
            - web
          middlewares:
            - redirect-to-https
            - badger

        next-router:
          rule: "Host(`pangolin.mcordell.dev`) && !PathPrefix(`/api/v1`)"
          service: next-service
          entryPoints:
            - websecure
          middlewares:
            - badger
          tls:
            certResolver: letsencrypt

        api-router:
          rule: "Host(`pangolin.mcordell.dev`) && PathPrefix(`/api/v1`)"
          service: api-service
          entryPoints:
            - websecure
          middlewares:
            - badger
          tls:
            certResolver: letsencrypt

        ws-router:
          rule: "Host(`pangolin.mcordell.dev`)"
          service: api-service
          entryPoints:
            - websecure
          middlewares:
            - badger
          tls:
            certResolver: letsencrypt

      services:
        next-service:
          loadBalancer:
            servers:
              - url: "http://pangolin:3002"

        api-service:
          loadBalancer:
            servers:
              - url: "http://pangolin:3000"

    tcp:
      serversTransports:
        pp-transport-v1:
          proxyProtocol:
            version: 1
        pp-transport-v2:
          proxyProtocol:
            version: 2
  '';

in
{
  # Enable Docker
  virtualisation.docker.enable = true;
  users.users.michael.extraGroups = [ "docker" ];

  # Pangolin config.yml (contains server secret from sops)
  sops.templates."pangolin-config.yml" = {
    content = ''
      gerbil:
        start_port: 51820
        base_endpoint: "pangolin.mcordell.dev"

      app:
        dashboard_url: "https://pangolin.mcordell.dev"
        log_level: "info"

      domains:
        domain1:
          base_domain: "mcordell.dev"
          cert_resolver: "letsencrypt"

      server:
        secret: "${config.sops.placeholder."pangolin/server_secret"}"
        cors:
          origins:
            - "https://pangolin.mcordell.dev"
          methods:
            - "GET"
            - "POST"
            - "PUT"
            - "DELETE"
            - "PATCH"
          allowed_headers:
            - "X-CSRF-Token"
            - "Content-Type"
          credentials: false

      flags:
        require_email_verification: false
        disable_signup_without_invite: true
        disable_user_create_org: true
        allow_raw_resources: true
        enable_integration_api: true
    '';
  };

  # Traefik env file (contains Cloudflare API token from sops)
  sops.templates."traefik.env" = {
    content = ''
      CF_DNS_API_TOKEN=${config.sops.placeholder."pangolin/cf_dns_api_token"}
    '';
  };

  # Systemd service for Pangolin Docker Compose stack
  systemd.services.pangolin = {
    description = "Pangolin Docker Compose Stack";
    after = [ "docker.service" "network-online.target" "sops-nix.service" ];
    wants = [ "network-online.target" ];
    requires = [ "docker.service" ];
    wantedBy = [ "multi-user.target" ];

    path = [ pkgs.docker ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      WorkingDirectory = dataDir;
    };

    preStart = ''
      # Create directory structure (preserves existing data)
      mkdir -p ${configDir}/traefik/logs
      mkdir -p ${configDir}/letsencrypt
      mkdir -p ${configDir}/db

      # Copy secret-bearing configs from sops
      cp -f ${config.sops.templates."pangolin-config.yml".path} ${configDir}/config.yml
      cp -f ${config.sops.templates."traefik.env".path} ${configDir}/traefik.env

      # Copy static traefik configs from nix store
      cp -f ${traefikStaticConfig} ${configDir}/traefik/traefik_config.yml
      cp -f ${traefikDynamicConfig} ${configDir}/traefik/dynamic_config.yml
    '';

    script = ''
      docker compose -f ${composeFile} --project-directory ${dataDir} up -d --wait
    '';

    postStop = ''
      docker compose -f ${composeFile} --project-directory ${dataDir} down
    '';
  };

  # Firewall: TCP 80/443 for web, UDP for WireGuard tunnels
  networking.firewall = {
    allowedTCPPorts = [ 80 443 ];
    allowedUDPPorts = [ 51820 21820 ];
  };
}
