{config, pkgs, ...}:
{
  boot = {
    kernelModules = ["kvm-intel"];
    extraModprobeConfig = "options kvm_intel nested=1";
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelPackages = pkgs.linuxPackages_latest;
  };
}
