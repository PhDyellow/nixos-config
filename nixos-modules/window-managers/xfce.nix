{config, pkgs, ...}:
{
  services.xserver = {
    enable = true;
    desktopManager = {
      xterm.enable = false;
      xfce = {
        enable = true;
        # noDesktop = true;
        # enableXfwm = false;
      };
    };
    # windowManager.i3 = {
    # enable = true;
    # extraPackages = with pkgs; [
    # dmenu
    # i3status
    # ];
    # };
    # dpi = 300;
  };
  services.displayManager.defaultSession = "xfce";
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet -t -r -g 'Init: Prime-AI' --cmd xfce";
        user = "phil";
      };
    };
  };
  # services.xserver.displayManager.lightdm.enable = true;
}
