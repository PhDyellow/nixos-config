{config, pkgs, lib, ...}:
{
  imports = [
    inputs.lanzaboote.nixosModules.lanzaboote
  ];
  #boot.bootspec.enable = true; #duplicated from prime-ai_hardware_config
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        enable = lib.mkForce false; #force to false for lanzaboote
        #enable = true;
        editor = false;  #don't allow kernel cli editing before boot
      };
    };
    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };
  };
}
