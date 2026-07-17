{config, pkgs, ...}:
{
  services.gnome.gnome-keyring.enable = true;
  environment.systemPackages = with pkgs; [
    grim
    slurp
    wl-clipboard
    mako
  ];
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };
}
