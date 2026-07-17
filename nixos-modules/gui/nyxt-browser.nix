{config, pkgs, ...}:
{
  environment.systemPackages = with pkgs; [
    nyxt
  ];
  environment.sessionVariables = {
    # Remove flickering in Nyxt browser
    WEBKIT_DISABLE_COMPOSITING_MODE=1;
  };
}
