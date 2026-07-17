{config, pkgs, ...}:
{
  environment.systemPackages = with pkgs; [
    inkscape
    # inkscape-with-extensions.override {
    #   inkscapeExtensions = [ inkscape-extensions.applytransforms ];
    # })
  ];
}
