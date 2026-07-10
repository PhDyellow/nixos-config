{config, pkgs, ...}:
{
  fonts = {
    packages = with pkgs; [
      nerd-fonts.fira-code
      nerd-fonts.roboto-mono
    ];
    fontconfig.defaultFonts = {
      monospace = [ "RobotoMono" ];
    };
  };
}
