{config, pkgs, ...}:
{
  environment.systemPackages = with pkgs; [
    bitwarden-desktop
    keepassxc
    (btop.override {cudaSupport = true;})
  ];
}
