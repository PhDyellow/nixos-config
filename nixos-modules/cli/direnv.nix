{config, pkgs, ... }:
{
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];
  ## Needed by direnv and nix-direnv to properly pin nix shells
  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
  };
  home-manager.users.phil = {
    programs.direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };
  };
}
