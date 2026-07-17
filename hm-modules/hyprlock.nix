{config, pkgs, ...}:
{
  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        hide_cursor = true;
        ignore_empty_input = true;
      };
      background = {
        monitor = "";
        color = "rgba(252, 229, 93, 1.0)";
      };
    };
  };
}
