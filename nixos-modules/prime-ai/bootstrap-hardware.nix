{config, pkgs, lib, modulesPath, ...}:
{

  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        editor = false;
      };
      efi.canTouchEfiVariables = true;
    };
    initrd = {
      luks.devices."nixos-crypt".device =
        "dev/disk/by-partlabel/NIXOSCRYPT";

      availableKernelModules = [
        "nvme"
        "xhci_pci"
        "ahci"
        "sdhci_pci"
      ];
      kernelModules = [ ];
    };

    supportedFilesystems = [
      "btrfs"
    ];
  };
  boot.initrd.postResumeCommands = lib.mkAfter ''
    mkdir /btrfs_tmp
    mount /dev/nixos_lvm_group/nix_persistent /btrfs_tmp
    if [[ -e /btrfs_tmp/root ]]; then
        mkdir -p /btrfs_tmp/old_roots
        timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
        mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
    fi

    delete_subvolume_recursively() {
        IFS=$'\n'
        for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
            delete_subvolume_recursively "/btrfs_tmp/$i"
        done
        btrfs subvolume delete "$1"
    }

    for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
        delete_subvolume_recursively "$i"
    done

    btrfs subvolume create /btrfs_tmp/root
    umount /btrfs_tmp
  '';

  fileSystems = {
    "/" = {
      device = "/dev/nixos_lvm_group/nix_persistent";
      fsType = "btrfs";
      options = [
        "noacl" # single user system
        "compress=zstd:8" # moderately aggressive compression
        "space_cache=v2"
        "discard=async" # Use trim occasionally
        "max_inline=28k" # 28k files are inlined.
        #I configured 32k nodes, so this is not too large
        "ssd" # force SSD
        "noatime" #reduce writes by not updating atimes on each read

        "subvol=root" # for impermanence
      ];
    };
    "/persistent" = {
      device = "dev/nixos_lvm_group/nix_persistent";
      neededForBoot = true;
      fsType = "btrfs";
      options = [
        "noacl" # single user system
        "compress=zstd:8" # moderately aggressive compression
        "space_cache=v2"
        "discard=async" # Use trim occasionally
        "max_inline=28k" # 28k files are inlined.
        #I configured 32k nodes, so this is not too large
        "ssd" # force SSD
        "noatime" #reduce writes by not updating atimes on each read

        "subvol=persistent" # persistent files for impermanence

      ];
    };

    "/boot" = {
      # device = "/dev/disk/by-partuuid/5a687aae-d3c0-4f4e-b580-5ce32bec51b2";
      device = "/dev/disk/by-label/EFIBOOT";
      fsType = "vfat";
      options = [
        "umask=077"
      ];
    };
  };

  swapDevices = [
    {
      label = "swap_hibernate";
      # Partition big enough for hibernation.
      # If compute is thrashing, kill compute,
      # batch, and restart.
      # If REALLY necessary, create a swapfile
      # in BTRFS on the fly, it should
      # get reclaimed by impermanence after restart.
    }
  ];
}
