{config, pkgs, ...}:
{
  hardware.sane.enable = true;
  users.users.phil.extraGroups = ["scanner" "lp" ];
}
