{config, pkgs, ...}:
{
  services.xserver.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.displayManager.defaultSession = "none+dwm";
  services.xserver.windowManager.dwm.enable = true;
}
