{ config, lib, pkgs, modulesPath, ...}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-pc-ssd
    inputs.nixos-hardware.nixosModules.common-pc
  ];
  # Dual booting with Windows requires one of the OS's
  # to change how they interpret the hardware clock
  time.hardwareClockInLocalTime = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        editor = false;
      };
      efi.canTouchEfiVariables = true;
    };
    #The next line may fix a system crash in nvidia 525.xx.xx
    #Nvidia has enabled a new feature in 510, GSP, but logs
    #show it was the cause of failure in my laptop.
    # options nvidia NVreg_EnableGpuFirmware=0
    extraModprobeConfig = ''
              options nvidia_drm modeset=1 fbdv=1


          '';
    initrd = {
      luks.devices."nixos_lvm".device =
        # "dev/disk/by-uuid/c4129dcf-90da-4d0c-8da9-880b9c111e6f";
        "dev/disk/by-partlabel/NIXOSCRYPT";
      availableKernelModules = [
        "nvme"
        "xhci_pci"
        "ahci"
        "sdhci_pci"
      ];
      kernelModules = [
        "nvidia"
        "nvidia_modeset"
        "nvidia_uvm"
        "nvidia_drm"
      ];
    };
    kernelModules = [
      "kvm-amd"
      "msr" #for zenstates
      #"k10temp"
      "cpuid"
      "lm92"
      "zenpower"
    ];
    blacklistedKernelModules = [
      "k10temp"
    ];
    extraModulePackages = [
      config.boot.kernelPackages.zenpower
    ];
    bootspec.enable = true; #needed for lanzaboote secureboot
    supportedFilesystems = [
      "ntfs" #needed for NTFS support
      "btrfs"
    ];
  };
  # Cleans out root on each boot, while saving
  # previous root to a snapshot and deleting
  # snapshots after a while
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
  boot.kernelParams = [
    "zswap.enable=1"
    "zswap.zpool=zsmalloc"
    "zswap.compressor=zstd"
    "zswap.max_pool_percent=60" #defaults to 20

    "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
  ];
  services.xserver.videoDrivers = ["nvidia"];
  hardware = {
    nvidia = {
      open = false;
      powerManagement.enable = true;
      modesetting.enable = true;
      #forceFullCompositionPipeline = true;
      nvidiaPersistenced = true;
      gsp.enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.beta;
    };
    graphics = {
      enable = true;
      extraPackages = with pkgs; [
        libva-vdpau-driver
        libvdpau-va-gl
        libva
        qt5.qtwayland
        qt6.qtwayland
        nvidia-vaapi-driver
      ];
    };
  };
  #services.cpupower-gui.enable = true;
  services.logind.settings.Login.HandleLidSwitch = "ignore";
  environment.systemPackages = with pkgs; [
    zenstates
    #ryzenadj #rejects the 5900X as "not mobile" and won't run
    linuxPackages.zenpower
    zenmonitor
    lm_sensors
    #psensor
    #amdctl #not in nixos, but does same job as zenstates
    #cpu-x
    mprime
  ];
}
