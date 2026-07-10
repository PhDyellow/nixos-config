{config, pkgs, ...}:
{
  services.flatpak.enable = true;
  users.users.phil.extraGroups = [ "flatpak" ];
}
