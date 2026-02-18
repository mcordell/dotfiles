{ config, ... }:
{
  services.pangolin = {
    enable = true;
    baseDomain = "mcordell.dev";
    dashboardDomain = "pangolin.mcordell.dev";
    letsEncryptEmail = "surpher@gmail.com";
    dnsProvider = "cloudflare";
    environmentFile = config.sops.templates."pangolin.env".path;
    openFirewall = true;

    settings = {
      flags = {
        disable_signup_without_invite = true;
        disable_user_create_org = true;
        require_email_verification = false;
      };
    };
  };
}
