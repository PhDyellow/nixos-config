{config, pkgs, ...}:
{
  environment.systemPackages = with pkgs; [
    firefox
    pqiv
    gthumb
  ];
}
