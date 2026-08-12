# Host-specific nix-darwin configuration for Michaels-MacBook-Pro.
{ inputs, user, ... }:
{
  # sherpa/sirup machine wiring. The module is a *home-manager* module, and
  # home-manager modules here get no `inputs` (mkHomeManagerConfig passes no
  # extraSpecialArgs), so the import is made from the darwin layer, which does.
  #
  # Enablement is deliberately per-host rather than in nix/home/darwin.nix: the
  # module's activation check refuses to wire launchd to a missing or stale
  # binary, so a mac that has not had `cargo install` run on it yet would fail
  # its own switch. Turn it on per machine as each is bootstrapped.
  home-manager.users.${user} = {
    imports = [ inputs.pimalaya.homeModules.sherpa ];

    # Account inventory, config paths, log paths and the launchd agents all
    # come from the module's own defaults — the account list must not live
    # here, since this repo is public.
    programs.sherpa.enable = true;
  };
}
