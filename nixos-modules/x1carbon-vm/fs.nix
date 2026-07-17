{config, pkgs, ...}:
{
  boot.initrd = {
    checkJournalingFS = false;
    availableKernelModules = [
      "xhci_pci"
      "ahci"
      "nvme"
      "sd_mod"
      "sr_mod"
    ];
    kernelModules = [];
  };
  boot = {
    kernelModules = [];
    extraModulePackages = [];
  };
  fileSystems = {
    "/para" = {
      fsType = "vboxsf";
      device = "para";
      options = ["rw" "uid=1001" "gid=100"];
    };
    "/" = {
      fsType = "btrfs";
      device = "/dev/disk/by-label/nixos";
    };
    "/boot" = {
      fsType = "vfat";
      device = "/dev/disk/by-partlabel/ESP";
      options = [
        "umask=077"
      ];
    };
  };
  swapDevices = [
    {
      device = "/dev/disk/by-label/swap";
    }
  ];

}
