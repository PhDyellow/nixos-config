{
  description = "Prime-AI Nixos Configuration";

  inputs  = {
    nixpkgs-unstable = {
      # url = "github:PhDyellow/nixpkgs/ryzen_smu";
      url = "github:NixOS/nixpkgs/nixos-unstable";
      #url = "github:NixOS/nixpkgs/master"; #temporary change for bug in nixos
      # url = "github:NixOS/nixpkgs?rev=5abc896edad307816c03d9a672cc8fcf683e8f35"; #temporary change for bug in nixos
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    ragenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    lanzaboote = {
      # url = "github:nix-community/lanzaboote?rev=f0cc345caa9eb3f8e033ddd74865f57b35825170";
      url = "github:nix-community/lanzaboote";
      inputs.nixpkgs.follows = "nixpkgs-unstable"; #needs unstable
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    # hyprland = {
      # url = "github:hyprwm/Hyprland?rev=c4dec4f79697cb789b585f8e4febf92e929b5291"; # failed to build
      # url = "github:hyprwm/Hyprland?rev=e43f7fc98defc3d9a5bc2fe249895d23e490392f"; #failed to build
      # url = "github:hyprwm/Hyprland?rev=f0e4f6622e3e9addc530119b804d2f71395455e7";
      #not following nixpkgs to get caching
    # };
    # tuxedo-nixos = {
    #   url = "github:blitz/tuxedo-nixos";
    #   # Avoid pulling in the nixpkgs that we pin in the tuxedo-nixos repo.
    #   # This should give the least surprises and saves on disk space.
    #   # inputs.nixpkgs.follows = "nixpkgs-unstable"; # not working with nixpkgs unstable yet
    # };

    openconnect-sso = {
      # url = "github:vlaci/openconnect-sso";
      url = "github:ThinkChaos/openconnect-sso/fix/nix-flake";
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid/release-23.05";
    };

    nur = {
      url = "github:nix-community/NUR";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      # url = "github:nix-community/emacs-overlay?rev=42a2a718bdcbe389e7ef284666d4aba09339a416";
      # url = "github:nix-community/emacs-overlay?rev=0acd590f3b518dfc8354bf9ed5c82e1401c4e6b0";
      #;https://github.com/nix-community/emacs-overlay/commit/e9e67599cda6f57f37178fd33ccff86cc2c2d6c4
      #url = "github:nix-community/emacs-overlay?rev=f57192297f370f8f01b1476023ca29caf032b20a";https://github.com/nix-community/emacs-overlay/commit/#start-of-content
    };

    emacs-overlay-pinned-android = {
      url = "github:nix-community/emacs-overlay";
      # url = "github:nix-community/emacs-overlay?rev=42a2a718bdcbe389e7ef284666d4aba09339a416";
      # url = "github:nix-community/emacs-overlay?rev=0acd590f3b518dfc8354bf9ed5c82e1401c4e6b0";
      #;https://github.com/nix-community/emacs-overlay/commit/e9e67599cda6f57f37178fd33ccff86cc2c2d6c4
      #url = "github:nix-community/emacs-overlay?rev=f57192297f370f8f01b1476023ca29caf032b20a";https://github.com/nix-community/emacs-overlay/commit/#start-of-content
    };


    pandoc-crossref = {
      url = "github:lierdakil/pandoc-crossref";
    };

    #Emacs packages
    org-sltypes = {
      url = "github:PhDyellow/org-sltypes/stable";
      flake = false;
    };
    org-slt-phdyellow = {
      url = "github:PhDyellow/org-slt-phdyellow/main";
      flake = false;
    };
    org-super-links = {
      url = "github:toshism/org-super-links";
      flake = false;
    };
    objed = {
      url = "github:PhDyellow/objed/main";
      flake = false;
    };
    key-game = {
      url = "gitlab:tygrdev/key-game";
      flake = false;
    };
    color-theme-buffer-local = {
      url = "github:matogoro/color-theme-buffer-local";
      flake = false;
    };
    isend-mode = {
      url = "github:PhDyellow/isend-mode.el";
      flake = false;
    };
    org-linker-edna = {
      url = "github:toshism/org-linker-edna";
      flake= false;
    };
    org-linker = {
      url = "github:toshism/org-linker";
      flake = false;
    };
    smart-tabs-mode = {
      url = "github:chep/smarttabs/macro-fix";
      flake = false;
    };
    denote = {
      url = "git+https://git.sr.ht/~protesilaos/denote";
      flake = false;
    };
    org-transclusion = {
      url = "github:nobiot/org-transclusion/main";
      flake = false;
    };
  };

  outputs = {self, nixpkgs-unstable, ...}@inputs: {
    nixosModules = {
      # NAMING SCHEME:
      # <device-name>.<module-name>: configuration specifically for one device, eg. prime-ai
      # <device-name>.hm: home-manager config for device
      # system-conf.<module-name>: system-wide configuration, not device specific
      #    likely to be shared by multiple programs, eg fonts
      # gui.<module-name>: a graphical program configured for my needs
      # cli.<module-name>: a cli program configured for my needs
      #   Only use gui or cli for programs that need tweaking

      system-conf = {
        # Programs that I want on all devices, and dont need to configure
        gui = {config, pkgs, ...}:
          {
            environment.systemPackages = with pkgs; [
              bitwarden
            ];
          };
        cli = {config, pkgs, ...}:
          {
            environment.systemPackages = with pkgs; [
              ripgrep
              nil # nix language server
              openssl
              vim
              git
              gitSVN
              wget
              curl
              dig
              pigz
              certbot
              squashfsTools
              squashfs-tools-ng
              inputs.ragenix.packages.x86_64-linux.default
            ];
          };
        allow-unfree = {config, pkgs, ...}:
          {
            nixpkgs.config.allowUnfree = true;
          };
        udisks = {config, pkgs, ...}:
          {
            services.udisks2.enable = true;
          };
        openssh = {config, pkgs, ...}:
          {
            #Enable OpenSSH daemon
            #Primary use here is for agenix.
            #Enabling openssh creates host keys, which agenix uses for secrets
            #password entry is therefore disabled, and firewall ports are not opened
            services.openssh = {
              enable = true;
              settings.PasswordAuthentication = false;
              openFirewall = false;
            };

            programs.ssh = {
              #agentTimeout = "1h"; #request passphrase for keys every hour
              startAgent = true;
              askPassword = "systemd-ask-password";
            };
          };
        secure_boot = {config, pkgs, lib, ...}:
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
          };
        network_fs = {config, pkgs, ...}:
          let
            # this line prevents hanging on network split
            automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=600,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,uid=1001,gid=100";
          in
            {
              age.secrets.cifs_dpbagje_share.file = ./agenix/cifs_dpbagje_share.age;
              fileSystems = {
                "/nas/dpbagj/parent_share" = {
                  device = "//100.108.81.63/parent_share";
                  fsType = "cifs";
                  options = ["${automount_opts},credentials=${config.age.secrets.cifs_dpbagje_share.path}"];
                };
                "/nas/dpbagj/family_share" = {
                  device = "//100.108.81.63/family_share";
                  fsType = "cifs";
                  options = ["${automount_opts},credentials=${config.age.secrets.cifs_dpbagje_share.path}"];
                };
              };
            };
        wifi_secrets = {config, pkgs, ...}:
          {
            age.secrets.wpa_pwd_env.file = ./agenix/wpa_pwd.env.age;
            networking.wireless = {
              environmentFile = config.age.secrets.wpa_pwd_env.path;
              networks = {
                PBAGJmob = {
                  psk = "@phone_psk@";
                  priority = 20;
                };
                WIFI-56E0-5G = {
                  psk = "@parent_psk@";
                  priority = 60;
                };
                WiFi-56E0-5G = {
                  psk = "@parent_psk@";
                  priority = 65;
                };
                PBAGJE_H_5G = {
                  psk = "@home_psk@";
                  priority = 99;
                };
                #BParent 2.4Ghz
                TelstraA76952 = {
                  psk = "@bparent_psk@";
                  priority = 50;
                };
              };
            };
          };

        locale_au = {config, pkgs, ...}:
          {
            time.timeZone = "Australia/Brisbane";
            i18n.defaultLocale = "en_AU.UTF-8";
          };
        fonts = {config, pkgs, ...}:
          {
            fonts = {
              packages = with pkgs; [
                (nerdfonts.override { fonts = [ "FiraCode" "RobotoMono" ]; })
              ];

              fontconfig.defaultFonts = {
                monospace = [ "RobotoMono" ];
              };
            };

          };

        lock-root = {config, pkgs, ...}:
          {
            users.mutableUsers = false;
            users.users = {
              root.hashedPassword = "*";
            };
          };
        nix-config = {config, pkgs, ...}:
          {

            nix = {
              package = pkgs.nixVersions.latest;
              settings = {
                system-features = [
                  "recursive-nix"
                  "kvm"
                  "big-parallel"
                  "nixos-test"
                  "benchmark"
                ];
                experimental-features = [
                  "nix-command"
                  "flakes"
                  "recursive-nix"
                ];
                substituters = [
                  "https://hyprland.cachix.org" # for hyprland
                  "https://nix-community.cachix.org" # for nix-community
                ];
                trusted-public-keys = [
                  "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" #for hyprland
                  "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" #For Nix-community
                ];
              };
            };
          };
        stateversion = {config, pkgs, ...}:
          {
            system.stateVersion = "21.11";
          };

        slurm-server = {config, pkgs, ...}:
          let
            gres = pkgs.writeTextFile {
              name = "gres.conf";
              text = ''
                Name=gpu Type=nvidia File=/dev/nvidia0
              '';
              destination = "/gres.conf";
            };
          in

            {
            environment.systemPackages = with pkgs; [
              apptainer
            ];
            age.secrets.munge_key = {
              file = ./agenix/munge_key.age;
              mode = "600";
              owner = "munge";
              group = "munge";
            };

            ## needed by slurm
            services.munge = {
              enable = true;
              password = config.age.secrets.munge_key.path;
            };
            services.slurm = {
              server = {
                enable = true;
              };
              client = {
                enable = true;
              };
              extraConfig = ''
                SlurmctldHost=${config.networking.hostName}

                SelectType=select/cons_tres
                SelectTypeParameters=CR_Core_Memory
                GresTypes=gpu
              '';
              extraConfigPaths = [
                "${gres}"
              ];
              # controlMachine = config.networking.hostName;
              # controlAddr="::1";
              nodeName = [
                "${config.networking.hostName} CPUs=11 CoresPerSocket=11 Sockets=1 RealMemory=62000  Gres=gpu:nvidia:1  State=UNKNOWN"
              ];
              partitionName = [
                "cpu Nodes=${config.networking.hostName} MaxCPUsPerNode=10 Default=YES MaxTime=UNLIMITED MaxMemPerNode=62000 State=UP"
                ## Reserve a cpu for the GPU jobs.
                "gpu Nodes=${config.networking.hostName} MaxCPUsPerNode=1  MaxTime=UNLIMITED MaxMemPerNode=62000 State=UP"
              ];
            };
          };
      };
      nix-on-droid-config = {


      };

      x1carbon-vm = {
        vm-settings = {config, pkgs, ...}: {
          virtualisation.virtualbox.guest.enable = true;
        };
        boot = {config, pkgs, ...}: {
          boot = {
            kernelModules = ["kvm-intel"];
            extraModprobeConfig = "options kvm_intel nested=1";
            loader.systemd-boot.enable = true;
            loader.efi.canTouchEfiVariables = true;
            kernelPackages = pkgs.linuxPackages_latest;
          };
        };
        networking = {config, pkgs, ...}: {
          networking = {
            firewall = {
              enable = true;
            };
          };
        };
        trackpad = {config, pkgs, ...}: {
          services.xserver.synaptics = {
            enable = true;
            twoFingerScroll = true;
          };
          # libinput is incompatible with synaptics, but is not disabled when
          # synaptics is enabled
          services.libinput.enable = false;
        };
        desktop = {config, pkgs, ...}: {
          services.xserver.enable = true;
          services.xserver.displayManager.lightdm.enable = true;
          services.displayManager.defaultSession = "none+dwm";
          services.xserver.windowManager.dwm.enable = true;
        };
        fs = {config, pkgs, ...}: {
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
            };
          };
          swapDevices = [
            {
              device = "/dev/disk/by-label/swap";
            }
          ];

        };
        phil_user = {config, pkgs, ...}: {
          age.secrets.user_phil_pwd_vm.file = ./agenix/user_phil_pwd_vm.age;
            users.users = {
              phil = {
                isNormalUser = true;
                extraGroups = ["wheel"];
                hashedPasswordFile = config.age.secrets.user_phil_pwd_vm.path;
                uid = 1001;
                shell = pkgs.fish;
              };
            };
            programs.fish.enable = true;

        };
      };
      prime-ai = {
        gui = {config, pkgs, ...}:
          {
            environment.systemPackages = with pkgs; [
              firefox
              nyxt
              pqiv
              gthumb
            ];
          };
        cli = {config, pkgs, ...}:
          {
            environment.systemPackages = with pkgs; [
            ];
          };

        networking = {config, pkgs, ...}:
          {
            environment.systemPackages = with pkgs; [
              openconnect
              inputs.openconnect-sso.packages.x86_64-linux.default
            ];
            networking = {
              hostName = "prime-ai-nixos";
              firewall = {
                enable = true;
                allowedTCPPorts = [ ];
                allowedUDPPorts = [ ];
              };
              wireless = {
                enable = true;
              };

              interfaces = {
                #enp4s0.useDHCP = true;
                #wlp5s0.useDHCP = true;
                # eno1 = {
                #   useDHCP = false;
                # };
                # wlp0s20f3 = {
                #   useDHCP = false;
                # };
                enp4s0 = {
                  useDHCP = true;
                };
                wlp5s0 = {
                  useDHCP = true;
                };
              };
            };

          };
        hardware_config_tuxedo = { config, lib, pkgs, modulesPath, ...}:
          let
            tailor-super-fans = pkgs.writeTextFile {
              name = "super-fans.json";
              text = ''
                [
                  {"temp":20,"fan":0},
                  {"temp":80,"fan":100},
                  {"temp":100,"fan":100}
                ]
              '';
            };
            tailor-super-profiles = pkgs.writeTextFile {
              name = "super.json";
              text = ''
                {
                  "fans":["super-fans","super-fans"],
                  "leds":[],
                  "performance_profile":"performance"
                }
              '';
            };
            tailor-super-activate = pkgs.writeTextFile {
              name = "active_profile.json";
              text = tailor-super-profiles.text;
            };
            tcc-profile = pkgs.writeTextFile {
              name = "tcc-profile";
              text =  ''
              [
                {
                  "id": "__default_custom_profile__",
                  "name": "TUXEDO Defaults",
                  "description": "Edit profile to change behaviour",
                  "display": {
                    "brightness": 100,
                    "useBrightness": false
                  },
                  "cpu": {
                         "useMaxPerfGov": false,
                         "governor": "performance",
                         "energyPerformancePreference": "performance",
                         "noTurbo": false,
                         "onlineCores": 24,
                         "scalingMinFrequency": 500000,
                         "scalingMaxFrequency": 4900000
                  },
                  "webcam": {
                    "status": true,
                    "useStatus": true
                  },
                  "fan": {
                    "useControl": true,
                    "fanProfile": "Balanced",
                    "minimumFanspeed": 0,
                    "offsetFanspeed": 0
                  },
                  "odmProfile": {
                    "name": "performance"
                  },
                  "odmPowerLimits": {
                    "tdpValues": []
                  }
                },
              {
                  "name": "freezy",
                  "description": "Edit profile to change behaviour",
                  "display": {
                    "brightness": 5,
                    "useBrightness": true
                  },
                  "cpu": {
                    "useMaxPerfGov": false,
                    "governor": "powersave",
                    "energyPerformancePreference": "performance",
                    "noTurbo": false,
                    "onlineCores": 24,
                    "scalingMinFrequency": 550000,
                    "scalingMaxFrequency": 5074000
                  },
                  "webcam": {
                    "status": false,
                    "useStatus": true
                  },
                  "fan": {
                    "useControl": true,
                    "fanProfile": "Freezy",
                    "minimumFanspeed": 0,
                    "offsetFanspeed": 5
                  },
                  "odmProfile": {
                    "name": "performance"
                  },
                  "odmPowerLimits": {
                    "tdpValues": []
                  },
                  "id": "0350erinz8o9lfg6puqi"
              }
              ]
            '';
            };
          in
            {
              imports = [
                # inputs.tuxedo-nixos.nixosModules.default
              ];
              # Tuxedo-rs is a rust-based GUI+CLI alternative to tuxedo-control-centre
              hardware.tuxedo-rs = {
                enable = true;
                tailor-gui.enable = true;
              };

              #powerManagement.cpuFreqGovernor = "performance"; #forced to schedutil by tuxedo control center
              # hardware.tuxedo-control-center.enable = true;
              systemd.services = {
                create-tcc-profile = {
                  serviceConfig.Type = "oneshot";
                  before = [ "tccd.service" ];
                  wantedBy = [ "multi-user.target" ];
                  script = ''
              mkdir -p /var/lib/tcc
              rm -f /var/lib/tcc/profiles
              ln -s ${tcc-profile} /var/lib/tcc/profiles
            '';
                };
              };
              systemd.services = {
                create-tailor-profile = {
                  serviceConfig.Type = "oneshot";
                  before = [ "tailord.service" ];
                  wantedBy = [ "multi-user.target" ];
                  script = ''
              mkdir -p /etc/tailord
              rm -f /etc/tailord/active_profile.json
              ln -s ${tailor-super-activate} /etc/tailord/active_profile.json
              mkdir -p /etc/tailord/profiles
              rm -f /etc/tailord/profiles/super.json
              ln -s ${tailor-super-profiles} /etc/tailord/profiles/super.json
              mkdir -p /etc/tailord/fan
              rm -f /etc/tailord/fan/super-fans.json
              ln -s ${tailor-super-fans} /etc/tailord/fan/super-fans.json
            '';
                };
              };
              # Redundant, done by enabling tuxedo-rs
              # hardware.tuxedo-keyboard.enable = true;
              # environment.systemPackages = [
              #   pkgs.linuxPackages.tuxedo-keyboard
              # ];
              # boot.kernelParams = [
              #   "tuxedo_keyboard.mode=0"
              #   "tuxedo_keyboard.brightness=10"
              #   "tuxedo_keyboard.color_left=0xff0a0a"
              # ];
              # Needed by tuxedo-nixos
              # Supposed to be set by tuxedo-nixos, but
              # not being seen for some reason
              # nixpkgs.config.permittedInsecurePackages = [
              #   "openssl-1.1.1u"
              #   "openssl-1.1.1t"
              #   "openssl-1.1.1w"
              #   "nodejs-14.21.3"
              #   "electron-13.6.9"
              # ];

            };
        hardware_config = { config, lib, pkgs, modulesPath, ...}:
          {
            imports = [
              (modulesPath + "/installer/scan/not-detected.nix")
              inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
              inputs.nixos-hardware.nixosModules.common-cpu-amd
              inputs.nixos-hardware.nixosModules.common-pc-ssd
              inputs.nixos-hardware.nixosModules.common-pc
              inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
            ];
            boot.kernelPackages = pkgs.linuxPackages_latest;

            boot = {
              loader = {
                systemd-boot.enable = true;
                efi.canTouchEfiVariables = true;
              };
              #The next line may fix a system crash in nvidia 525.xx.xx
              #Nvidia has enabled a new feature in 510, GSP, but logs
              #show it was the cause of failure in my laptop.
              extraModprobeConfig = ''
            options nvidia NVreg_EnableGpuFirmware=0

          '';
              initrd = {
                luks.devices."nixos-crypt".device =
                  "dev/disk/by-uuid/c4129dcf-90da-4d0c-8da9-880b9c111e6f";

                availableKernelModules = [
                  "nvme"
                  "xhci_pci"
                  "ahci"
                  "sdhci_pci"
                ];
                kernelModules = [ ];
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
                config.boot.kernelPackages.zenpower];
              bootspec.enable = true; #needed for lanzaboote secureboot
              supportedFilesystems = [
                "ntfs" #needed for NTFS support
                "btrfs"
              ];
            };
            fileSystems = {
              "/" = {
                device = "/dev/mapper/nixos-crypt";
                fsType = "btrfs";
              };
              "/boot" = {
                device = "/dev/disk/by-partuuid/5a687aae-d3c0-4f4e-b580-5ce32bec51b2";
                fsType = "vfat";
              };
            };
            #Swapfile
            #Swapfile is created by
            #sudo btrfs subvolume create /swap
            systemd.services = {
              create-swapfile = {
                serviceConfig.Type = "oneshot";
                wantedBy = [ "swap-swapfile.swap" ];
                script = ''
              swapfile="/swap/swapfile"
              if [[ -f "$swapfile" ]]; then
              echo "Swap file $swapfile already exists, taking no action"
              else
              echo "Setting up swap file $swapfile"
              ${pkgs.coreutils}/bin/truncate -s 0 "$swapfile"
              ${pkgs.e2fsprogs}/bin/chattr +C "$swapfile"
              fi
            '';
              };
            };
            swapDevices = [
              {
                device = "/swap/swapfile";
                # 8GB swap. RAM is AT LEAST 5x faster
                # than M2 NVME. Almost always faster to
                # just OOM, then restart the analysis
                # either batched or sequentially.
                size = (1024 * 1) * 8;
              }
            ];
            boot.kernelParams = ["resume_offset=4503599627370495"];
            boot.resumeDevice = "/dev/mapper/nixos-crypt";
            services.xserver.videoDrivers = ["nvidia"];
            hardware = {
              nvidia = {
                #open = true;
                powerManagement.enable = true;
                modesetting.enable = true;
                #forceFullCompositionPipeline = true;
                nvidiaPersistenced = true;
              };
              opengl = {
                enable = true;
                driSupport = true;
                driSupport32Bit = true;
                extraPackages = with pkgs; [
                  vaapiVdpau
                  libvdpau-va-gl
                  libva
                  qt5.qtwayland
                  nvidia-vaapi-driver
                ];
              };
            };
            #services.cpupower-gui.enable = true;
            services.logind.lidSwitch = "ignore";
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
          };


        hardware_shared_crypt = { config, lib, pkgs, ...}:
          {
            fileSystems = {
              #tested ntfs-3g and ntfs3 with
              #dd if=/dev/urandom of=/para/test.bad oflag=direct count=32k bs=128k
              #ntfs-3g (472, 448, 469)Mb/s
              #ntfs3 (323, 324, 329)Mb/r
              #ntfs-3g is faster!
              "/para" = {
                device = "/dev/mapper/para-crypt"; #after mounting from crypttab
                fsType = "ntfs-3g";
                options = [ "rw"
                            "uid=1001"
                            "gid=100"
                            "windows_names" #added to kernel in 6.2, current kernel in NixOS is 6.1.9. Supported by ntfs-3g
                            "fmask=133"
                            "dmask=022"
                            "norecover" # this is an ntfs-3g option, not supported by ntfs3
                            #"discard" #ntfs3 only option
                          ];
              };
            };
            environment.etc.crypttab = {
              enable = true;
              text = ''
            para-crypt /dev/disk/by-partuuid/1b5333c3-9421-44d5-8d21-fc2f22c8cbe3 /secrets/bitlocker/para.bek bitlk
          '';
            };
          };
        bootstrap_hardware = {config, pkgs, modulesPath, ...}:
          {

            imports = [
              (modulesPath + "/installer/scan/not-detected.nix")
            ];
            boot = {
              loader = {
                systemd-boot.enable = true;
                efi.canTouchEfiVariables = true;
              };
              initrd = {
                luks.devices."nixos-crypt".device =
                  "dev/disk/by-uuid/c4129dcf-90da-4d0c-8da9-880b9c111e6f";

                availableKernelModules = [
                  "nvme"
                  "xhci_pci"
                  "ahci"
                  "sdhci_pci"
                ];
                kernelModules = [ ];
              };

              supportedFilesystems = [
                "ntfs" #needed for NTFS support
                "btrfs"
              ];
            };
            fileSystems = {
              "/" = {
                device = "/dev/mapper/nixos-crypt";
                fsType = "btrfs";
              };
              "/boot" = {
                device = "/dev/disk/by-partuuid/5a687aae-d3c0-4f4e-b580-5ce32bec51b2";
                fsType = "vfat";
              };
            };
          };

        bootstrap_user = {config, pkgs, ...}:
          {
            users.users = {
              bootstrap = {
                isNormalUser = true;
                extraGroups = ["wheel"];
                password = "tmppwd";
              };
            };
          };
        syncthing = {config, pkgs, ...}:
          {
            services.syncthing = {
              enable = true;
              dataDir = "/home/phil/syncthing";
              configDir = config.services.syncthing.dataDir + "/.config/syncthing";
              overrideDevices = true;
              overrideFolders = true;
              user = "phil";
              group = "users";
              openDefaultPorts = true;
              settings = {
                devices = {
                  dpbagje = {
                    addresses = [
                      "quic://192.168.20.10:22000"
                      "quic://dpbagje.philjd.com:22000"
                    ];
                    id = "V2CZC46-XWNXBME-WDTOBSM-SHIO25H-KTTUFYQ-BXRRXFM-PQFYLYK-LUGCBQK";
                  };
                  galaxy_m62 = {
                    addresses = [
                      "quic://100.89.185.54:22000"
                      "quic://192.168.20.14:22000"
                      "quic://192.168.1.119:22000"
                    ];
                    id = "O4OCDD3-BN3WGHU-4U42GOR-CZQQTSQ-GTSULNM-YQM76V5-6R7RT2Y-TTOG5AG";
                  };
                  x1_carbon = {
                    addresses = [
                      "quic://100.103.6.30:2200"
                    ];
                    id = "PBQHAZ3-VEXG3K6-VC7AHMS-3OPLJOJ-SIL4UFP-MRIPZHL-PS2DUTD-DO6QXA6";
                  };
                };
                folders = {
                  memx = {
                    path = "/para/areas/memx___syncthing/";
                    id = "nihsu-jd7zf";
                    enable = true;
                    devices = [
                      "dpbagje"
                      "galaxy_m62"
                      "x1_carbon"
                    ];
                  };
                  memx_transition = {
                    path = "/para/resources/memx___syncthing__transition";
                    id = "raehb-7gn4q";
                    enable = true;
                    devices = [
                      "dpbagje"
                      "galaxy_m62"
                      "x1_carbon"
                    ];
                  };
                  manage_time_transition = {
                    path = "/para/resources/manage_time___syncthing__transition";
                    id = "tg7ol-vf4xc";
                    enable = true;
                    devices = [
                      "dpbagje"
                      "galaxy_m62"
                      "x1_carbon"
                    ];
                  };
                  transfer_sync = {
                    path = "/para/resources/transfer_sync___syncthing";
                    id = "0nae2-zo3f7";
                    enable = true;
                    devices = [
                      "dpbagje"
                      "galaxy_m62"
                      "x1_carbon"
                    ];
                  };
                  reading_transition = {
                    path = "/para/resources/reading___syncthing__transition/";
                    id = "sy3q4-6cput";
                    enable = true;
                    devices = [
                      "dpbagje"
                      "x1_carbon"
                    ];
                  };
                  zettlekasten_transition = {
                    path = "/para/resources/zettlekasten___syncthing__transition/";
                    id = "wsbyx-rus2l";
                    enable = true;
                    devices = [
                      "dpbagje"
                      "x1_carbon"
                    ];
                  };
                  zotfile_storage_transition = {
                    path = "/para/resources/zotfile_storage___syncthing__transition/";
                    id = "jzunu-qsesd";
                    enable = true;
                    devices = [
                      "dpbagje"
                      "x1_carbon"
                    ];
                  };
                  phd_transition = {
                    path = "/para/projects/phd___syncthing__transition/";
                    id = "nbdjg-farns";
                    enable = true;
                    devices = [
                      "x1_carbon"
                    ];
                  };
                  phd_draft = {
                    path = "/para/projects/phd_draft___syncthing/";
                    id = "ceto7-fdkqr";
                    enable = true;
                    devices = [
                      "x1_carbon"
                    ];
                  };
                  synology_nas_admin = {
                    path = "/para/projects/synology_nas_admin___syncthing/";
                    id = "rfra6-xulze";
                    enable = true;
                    devices = [
                      "x1_carbon"
                      "dpbagje"
                    ];
                  };
                };
              };
              cert = "/secrets/syncthing/cert.pem";
              key = "/secrets/syncthing/key.pem";
            };
          };
        tailscale = {config, pkgs, ...}:
          {
            age.secrets.prime_ai_tailscale.file = ./agenix/prime_ai_tailscale.age;
            services.tailscale = {
              enable = true;
            };
            # https://tailscale.com/blog/nixos-minecraft/
            # create a oneshot job to authenticate to Tailscale
            systemd.services.tailscale-autoconnect = {
              description = "Automatic connection to Tailscale";

              # make sure tailscale is running before trying to connect to tailscale
              after = [ "network-pre.target" "tailscale.service" ];
              wants = [ "network-pre.target" "tailscale.service" ];
              wantedBy = [ "multi-user.target" ];

              # set this service as a oneshot job
              serviceConfig.Type = "oneshot";

              # have the job run this shell script
              script = with pkgs; ''
            # wait for tailscaled to settle
            sleep 2

            # check if we are already authenticated to tailscale
            status="$(${tailscale}/bin/tailscale status -json | ${jq}/bin/jq -r .BackendState)"
            if [ $status = "Running" ]; then # if so, then do nothing
            exit 0
            fi

            # otherwise authenticate with tailscale
            ${tailscale}/bin/tailscale up -authkey file:${config.age.secrets.prime_ai_tailscale.path}
          '';
            };

          };
        phil_user = {config, pkgs, ...}:
          {
            age.secrets.user_phil_pwd.file = ./agenix/user_phil_pwd.age;
            users.users = {
              phil = {
                isNormalUser = true;
                extraGroups = ["wheel"];
                hashedPasswordFile = config.age.secrets.user_phil_pwd.path;
                uid = 1001;
                shell = pkgs.fish;
              };
            };
            programs.fish.enable = true;
          };

        phil_home = {config, pkgs, ...}: {
          imports = [
            inputs.home-manager.nixosModules.home-manager
          ];

          # This is the place to target emacs program versions
          nixpkgs.overlays = [
            inputs.emacs-overlay.overlays.default
          ];


          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            #config inserted before use-package
            users.phil = {
              imports = let
                nurNoPkgs = import inputs.nur { pkgs = null; nurpkgs = pkgs; };
              in [
                nurNoPkgs.repos.rycee.hmModules.emacs-init
                self.hmModules.emacs-hm-init
                # self.hmModules.emacs-mwe
                self.hmModules.gtk_setup
                self.hmModules.gpg-agent-emacs
                self.hmModules.hyprland-config
                self.hmModules.git-config
                self.hmModules.r-config
                self.hmModules.tex-full
              ];
              manual.manpages.enable = false;

              home = {
                stateVersion = "23.05";
                file = {
                  enchant-ordering = {
                    target = ".config/enchant/enchant.ordering";
                    text = ''
                    en_AU:aspell,nuspell
                    en:aspell,nuspell
                    en_GB:aspell,nuspell'';
                  };
                };
              };

              services = {
                recoll = {
                  enable = true;
                  ## doesn't build, fix should be easy but
                  ## time is more valuable than avoiding an unused GUI
                  # package = (pkgs.recoll.override {withGui = false;});
                  settings = {
                    nocjk = true;
                    loglevel = 5;
                    underscorasletter = true;
                    cachedir = "/para/resources/recoll___cache/";
                    aspellLanguage = "en";
                    topdirs = [
                      "/para/archives"
                      "/para/projects"
                      "/para/resources"
                      "/para/areas"
                    ];
                    "/para/resources" = {
                      skippedNames = [ "recoll___cache" ];
                    };
                  };
                };
              };

              programs = {
                bash = {
                  enable = true;
                };
                htop = {
                  enable = true;
                  ## Htop is getting frequent updates
                  ## Currently htop has a new interface that home-manager
                  ## does not support. A PR is waiting merge.
                  ## until then, settings are inputed manually.
                  settings = {
                    # Defaults
                    htop_version="3.3.0";
                    config_reader_min_version=3;
                    hide_kernel_threads=1;
                    hide_userland_threads=0;
                    hide_running_in_container=0;
                    shadow_other_users=0;
                    show_thread_names=0;
                    show_program_path=0;
                    highlight_deleted_exe=1;
                    shadow_distribution_path_prefix=0;
                    highlight_megabytes=1;
                    highlight_threads=1;
                    highlight_changes=0;
                    highlight_changes_delay_secs=5;
                    find_comm_in_cmdline=1;
                    strip_exe_from_cmdline=1;
                    show_merged_command=0;
                    screen_tabs=1;
                    header_margin=1;
                    detailed_cpu_time=0;
                    cpu_count_from_one = 0;
                    degree_fahrenheit=0;
                    update_process_names=0;
                    account_guest_in_cpu_meter=0;
                    # End of Defaults

                    # General config

                    delay=15; #default
                    hide_function_bar=0;
                    show_cpu_usage=1;
                    show_cpu_frequency=1;
                    show_cpu_temperature=1;

                    enable_mouse=0;
                    highlight_base_name=1;


                    # Meters
                    header_layout="two_50_50";
                    column_meters_0="LeftCPUs DiskIO NetworkIO System DateTime Battery Hostname MemorySwap";
                    column_meter_modes_0="1 2 2 2 2 2 2 1";
                    column_meters_1="RightCPUs PressureStallCPUSome PressureStallIOSome PressureStallIOFull PressureStallIRQFull PressureStallMemorySome PressureStallMemoryFull";
                    column_meter_modes_1="1 2 2 2 2 2 2";

                    # Hack nixos
                    color_scheme=''6 # 0,1,5 ok 6 good
# Screens
screen:BY_CPU=PID USER PRIORITY NICE ELAPSED OOM STATE PERCENT_CPU PERCENT_MEM Command
.sort_key=PERCENT_CPU
.tree_sort_key=PID
.tree_view_always_by_pid=0
.tree_view=0
.sort_direction=-1
.tree_sort_direction=1
.all_branches_collapsed=0
screen:BY_MEM=PID USER PRIORITY NICE ELAPSED OOM STATE PERCENT_CPU PERCENT_MEM Command
.sort_key=PERCENT_MEM
.tree_sort_key=PID
.tree_view_always_by_pid=0
.tree_view=0
.sort_direction=-1
.tree_sort_direction=1
.all_branches_collapsed=0
screen:BY_RUNTIME=PID USER PRIORITY NICE ELAPSED OOM STATE PERCENT_CPU PERCENT_MEM Command
.sort_key=ELAPSED
.tree_sort_key=PID
.tree_view_always_by_pid=0
.tree_view=0
.sort_direction=-1
.tree_sort_direction=1
.all_branches_collapsed=0
screen:I/O=PID USER IO_PRIORITY IO_RATE IO_READ_RATE IO_WRITE_RATE PERCENT_SWAP_DELAY PERCENT_IO_DELAY Command
.sort_key=IO_RATE
.tree_sort_key=PID
.tree_view_always_by_pid=0
.tree_view=0
.sort_direction=-1
.tree_sort_direction=1
.all_branches_collapsed=0
screen:TREE=PID PPID USER Command
.sort_key=PID
.tree_sort_key=PPID
.tree_view_always_by_pid=0
.tree_view=1
.sort_direction=-1
.tree_sort_direction=1
.all_branches_collapsed=0
'';
                  };
                };
                fish = {
                  enable = true;
                };
                nushell = {
                  enable = true;
                };
                gpg = {
                  enable = true;
                };
                ssh = {
                  enable = true;
                  matchBlocks = {
                    rdm = {
                      hostname = "data.qriscloud.org.au";
                      user = "uqpdyer";
                      forwardX11Trusted = true;
                      identitiesOnly = true;
                      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
                    };
                    getafix = {
                      hostname = "getafix.smp.uq.edu.au";
                      user = "uqpdyer";
                      forwardX11Trusted = true;
                      identitiesOnly = true;
                      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
                      port = 2022;
                    };
                    getafix0 = {
                      hostname = "getafix1.smp.uq.edu.au";
                      user = "uqpdyer";
                      forwardX11Trusted = true;
                      identitiesOnly = true;
                      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
                      port = 2022;
                    };
                    getafix1 = {
                      hostname = "getafix2.smp.uq.edu.au";
                      user = "uqpdyer";
                      forwardX11Trusted = true;
                      identitiesOnly = true;
                      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
                      port = 2022;
                    };
                    github = {
                      hostname = "github.com";
                      identitiesOnly = true;
                      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
                    };
                    dogmatix = {
                      hostname = "dogmatix.smp.uq.edu.au";
                      user = "uqpdyer";
                      forwardX11Trusted = true;
                      identitiesOnly = true;
                      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
                    };

                  };
                };
              };
            };
          };
        };

      };



      #ssh_public_config

      window-managers = {
        xfce_desktop = {config, pkgs, ...}:
          {
            services.xserver = {
              enable = true;
              desktopManager = {
                xterm.enable = false;
                xfce = {
                  enable = true;
                  # noDesktop = true;
                  # enableXfwm = false;
                };
              };
              # windowManager.i3 = {
              # enable = true;
              # extraPackages = with pkgs; [
                # dmenu
                # i3status
              # ];
              # };
              # dpi = 300;
            };
            services.displayManager.defaultSession = "xfce";
            services.greetd = {
              enable = true;
              settings = {
                default_session = {
                  command = "${pkgs.greetd.tuigreet}/bin/tuigreet -t -r -g 'Init: Prime-AI' --cmd Hyprland";
                  user = "phil";
                };
              };
            };
            # services.xserver.displayManager.lightdm.enable = true;
          };

        hyprland = {config, pkgs, ...}:
          {

            nixpkgs.overlays = [
              self.overlays.lsix_configured
            ];

            programs.hyprland = {
              enable = true;
            };


            services.greetd = {
              enable = true;
              settings = {
                default_session = {
                  command = "${pkgs.greetd.tuigreet}/bin/tuigreet -t -r -g 'Init: Prime-AI' --cmd Hyprland";
                  user = "phil";
                };
              };
            };
            environment = {

              systemPackages = with pkgs; [
                pipewire #Audio
                wireplumber
                fnott #desktop notifications. see also mako, dunst
                polkit #request root priveliges
                polkit_gnome #gnome app for polkit requests
                waylock
                #swaylock
                swayimg
                kitty
                foot
                lsix
                libsixel
                wl-clipboard
              ];
              sessionVariables = {
                _JAVA_AWT_WM_NONREPARENTING="1";
                XCURSOR_SIZE="24";
                # NIXOS_OZONE_WL = "1"; #Already set by hyprland module
                LIBVA_DRIVER_NAME="nvidia";
                XDG_SESSION_TYPE = "wayland";
                GBM_BACKEND = "nvidia-drm";
                __GLX_VENDOR_LIBRARY_NAME = "nvidia";
                WLR_NO_HARDWARE_CURSORS = "1";
                HYPRLAND_LOG_WLR="1";
                # __GL_GSYNC_ALLOWED = "0";
                # __GL_VRR_ALLOWED = "0";
                # DISABLE_QT5_COMPAT = "0";
                # ANKI_WAYLAND = "1";
                # DIRENV_LOG_FORMAT = "";
                # WLR_DRM_NO_ATOMIC = "1";
                # QT_QPA_PLATFORM = "wayland";
                # QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
                # QT_QPA_PLATFORMTHEME = "qt5ct";
                # MOZ_ENABLE_WAYLAND = "1";
                # WLR_BACKEND = "vulkan";
                # CLUTTER_BACKEND = "wayland";
                # WLR_DRM_DEVICES = "/dev/dri/card1:/dev/dri/card0";
              };
            };

            #pipewire specific config
            security.rtkit.enable = true;
            services.pipewire = {
              enable = true;
              alsa.enable = true;
              alsa.support32Bit = true;
              pulse.enable = true;
              # If you want to use JACK applications, uncomment this
              #jack.enable = true;
            };


            #Enable polkit for passwords, and activate agent
            security.polkit.enable = true;
            security.pam.services = {
              swaylock = {};
              waylock = {};
            };
            systemd = {
              user.services.polkit-gnome-authentication-agent-1 = {
                description = "polkit-gnome-authentication-agent-1";
                wantedBy = [ "graphical-session.target" ];
                wants = [ "graphical-session.target" ];
                after = [ "graphical-session.target" ];
                serviceConfig = {
                  Type = "simple";
                  ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
                  Restart = "on-failure";
                  RestartSec = 1;
                  TimeoutStopSec = 10;
                };
              };
            };

          };
      };
      cli = {
        ryzen_monitor = {config, pkgs, ...}:
          {
            programs.ryzen-monitor-ng.enable = true;
          };

        # System-wide python
        python-system = {config, pkgs, ...}:
          {
            environment.systemPackages = with pkgs; [
              (python3.withPackages(ps: with ps; [
                # inkex
              ]))
            ];
          };

        spell_checkers = {config, pkgs, ...}: {
          environment = {
            sessionVariables = {
              ENCHANT_CONFIG_DIR="/home/phil/.config/enchant";
            };
            systemPackages = with pkgs; [
              (aspellWithDicts (dicts: with dicts; [ en en-computers en-science]))
              hunspellDicts.en-au-large

              enchant

              (nuspellWithDicts [
                hunspellDicts.en-au-large
              ])
            ];

          };
        };
        direnv = {config, pkgs, ... }: {
          imports = [
            inputs.home-manager.nixosModules.home-manager
          ];
          ## Needed by direnv and nix-direnv to properly pin nix shells
          nix.settings = {
            keep-outputs = true;
            keep-derivations = true;
          };
          home-manager.users.phil = {
            programs.direnv = {
              enable = true;
              nix-direnv = {
                enable = true;
              };
            };
          };
        };
      };
      gui = {
        inkscape = {config, pkgs, ...}: {
          environment.systemPackages = with pkgs; [
            inkscape
            # inkscape-with-extensions.override {
            #   inkscapeExtensions = [ inkscape-extensions.applytransforms ];
            # })
          ];
        };
      };

      bib_reorganise = {config, pkgs, ...}: {

        systemd.timers."bib_reorganise" = {
          description = "Move new bib entries to main collection to improve caching";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnBootSec = "2h";
            OnUnitActiveSec = "2h";
            Unit = "bib-reorganise";
          };
        };
        systemd.services."bib_reorganise" = {

          # set this service as a oneshot job
          serviceConfig = {
            Type = "oneshot";
          };

          # have the job run this shell script
          script = with pkgs; ''
                #!/bin/bash

                # large, changes less frequently
                BIG=/para/areas/bibliography___CITE/readings.bib
                BIG2=/para/areas/bibliography___CITE/readings2.org
                # small, changes more frequently
                SMALL=/para/areas/bibliography___CITE/new_refs.bib
                SMALL2=/para/areas/bibliography___CITE/new_refs2.org

                # size of small before script actually does anything
                MAXSIZE=5000

                # get file size
                FILESIZE=$(stat -c%s "$SMALL")
                FILESIZE2=$(stat -c%s "$SMALL2")

                if ((FILESIZE > MAXSIZE)); then
                    # when $SMALL exceeds $MAXSIZE, move its content to $BIG
                    cat "$SMALL" >> "$BIG"
                    echo "" > "$SMALL"
                fi

                if ((FILESIZE2 > MAXSIZE)); then
                    # when $SMALL2 exceeds $MAXSIZE, move its content to $BIG2
                    cat "$SMALL2" >> "$BIG2"
                    echo "" > "$SMALL2"
                fi
              '';
        };
      };
    };
    #Modules for importing into home-manager.users.<name>.imports = [ here ];
    hmModules = {
      r-config = {config, pkgs, ...}: {
        home.file.r-config = {
          target = ".Rprofile";
          text = ''
                    ##Always install from csiro cran mirror when calling install.packages()
                    local({r <- getOption("repos")
                    r["CRAN"] <- "https://cran.csiro.au/"
                    options(repos=r)})

                    ##Dont ask to save workspace...actually, really I should just alias R to "R --no-save"
                    #q <- function (save="no", ...) {
                    #        quit(save=save, ...)}

                    #Stop Rstudio from checking for updates. Leave that to the nix shell
                    RSTUDIO_DISABLE_CHECK_FOR_UPDATES=1

                    ##Get tab completion on library names
                    utils::rc.settings(ipck=TRUE)

                  '';
        };

      };
      git-config = {config, pkgs, ...}: {
        programs.git ={
          enable = true;
          package = pkgs.gitAndTools.gitFull;
          userName = "Phil Dyer";
          userEmail = "phildyer@protonmail.com";
          extraConfig = {
            core = {
              autocrlf = "input";
            };
            github.user = "PhDyellow";
          };
        };
      };
      hyprland-config = {config, pkgs, ...}: {
        home.file = {
          hyprland-config = {
            target = ".config/hypr/hyprland.conf";
            text = ''
                # This is an example Hyprland config file.
                #
                # Refer to the wiki for more information.

                #
                # Please note not all available settings / options are set here.
                # For a full list, see the wiki
                #

                # See https://wiki.hyprland.org/Configuring/Monitors/
                monitor=eDP-1,2560x1440@165,0x0,1


                # See https://wiki.hyprland.org/Configuring/Keywords/ for more

                # Execute your favorite apps at launch
                # exec-once = waybar & hyprpaper & firefox

                # Source a file (multi-file configs)
                # source = ~/.config/hypr/myColors.conf

                # Some default env vars.
                env = XCURSOR_SIZE,24

                # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
                input {
                kb_layout = us
                kb_variant =
                kb_model =
                kb_options =
                kb_rules =

                follow_mouse = 2

                touchpad {
                natural_scroll = false
                }

                sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
                }

                general {
                # See https://wiki.hyprland.org/Configuring/Variables/ for more

                gaps_in = 5
                gaps_out = 20
                border_size = 2
                col.active_border = rgba(33ccffee) rgba(00ff99ee) 45deg
                col.inactive_border = rgba(595959aa)

                layout = dwindle

                no_cursor_warps = true
                }

                decoration {
                # See https://wiki.hyprland.org/Configuring/Variables/ for more

                rounding = 10
                  blur {
                    enabled = true
                    size = 3
                    passes = 1
                    new_optimizations = true
                  }
                drop_shadow = true
                shadow_range = 4
                shadow_render_power = 3
                col.shadow = rgba(1a1a1aee)
                }

                animations {
                enabled = true

                # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

                bezier = myBezier, 0.05, 0.9, 0.1, 1.05

                animation = windows, 1, 7, myBezier
                animation = windowsOut, 1, 7, default, popin 80%
                animation = border, 1, 10, default
                animation = borderangle, 1, 8, default
                animation = fade, 1, 7, default
                animation = workspaces, 1, 6, default
                }

                dwindle {
                # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
                pseudotile = true # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
                preserve_split = true # you probably want this
                }

                master {
                # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
                new_is_master = true
                }

                gestures {
                # See https://wiki.hyprland.org/Configuring/Variables/ for more
                workspace_swipe = false
                }

                misc {
                enable_swallow = true
                swallow_regex = ^(Alacritty|kitty|foot)$
                force_default_wallpaper = 0
                }


                # Example per-device config
                # See https://wiki.hyprland.org/Configuring/Keywords/#executing for more
                #device:epic mouse V1 {
                #    sensitivity = -0.5
                #}

                # trigger when the switch is toggled
                #bindl=,switch:Lid Switch,exec,waylock
                bindl=,switch:on:Lid Switch,exec,waylock
                # trigger when the switch is turning on
                #bindl=,switch:on:Lid Switch,exec,hyprctl keyword monitor "eDP-1,2560x1440@165,0x0,1"
                # trigger when the switch is turning off
                #bindl=,switch:off:Lid Switch,exec,hyprctl keyword monitor "eDP-1, disable"

                # Example windowrule v1
                # windowrule = float, ^(kitty)$
                # Example windowrule v2
                # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
                # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more


                # See https://wiki.hyprland.org/Configuring/Keywords/ for more
                $mainMod = SUPER

                # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
                bind = $mainMod, Q, exec, foot
                bind = $mainMod, C, killactive,
                bind = $mainMod, M, exit,
                bind = $mainMod, L, exec, waylock
                bind = $mainMod, V, togglefloating,
                bind = $mainMod, R, exec, wofi --show drun
                bind = $mainMod, P, pseudo, # dwindle
                bind = $mainMod, J, togglesplit, # dwindle


                # Move focus with mainMod + arrow keys
                bind = $mainMod, left, movefocus, l
                bind = $mainMod, right, movefocus, r
                bind = $mainMod, up, movefocus, u
                bind = $mainMod, down, movefocus, d

                # Switch workspaces with mainMod + [0-9]
                bind = $mainMod, 1, workspace, 1
                bind = $mainMod, 2, workspace, 2
                bind = $mainMod, 3, workspace, 3
                bind = $mainMod, 4, workspace, 4
                bind = $mainMod, 5, workspace, 5
                bind = $mainMod, 6, workspace, 6
                bind = $mainMod, 7, workspace, 7
                bind = $mainMod, 8, workspace, 8
                bind = $mainMod, 9, workspace, 9
                bind = $mainMod, 0, workspace, 10

                # Move active window to a workspace with mainMod + SHIFT + [0-9]
                bind = $mainMod SHIFT, 1, movetoworkspace, 1
                bind = $mainMod SHIFT, 2, movetoworkspace, 2
                bind = $mainMod SHIFT, 3, movetoworkspace, 3
                bind = $mainMod SHIFT, 4, movetoworkspace, 4
                bind = $mainMod SHIFT, 5, movetoworkspace, 5
                bind = $mainMod SHIFT, 6, movetoworkspace, 6
                bind = $mainMod SHIFT, 7, movetoworkspace, 7
                bind = $mainMod SHIFT, 8, movetoworkspace, 8
                bind = $mainMod SHIFT, 9, movetoworkspace, 9
                bind = $mainMod SHIFT, 0, movetoworkspace, 10

                # Scroll through existing workspaces with mainMod + scroll
                bind = $mainMod, mouse_down, workspace, e+1
                bind = $mainMod, mouse_up, workspace, e-1

                # Move/resize windows with mainMod + LMB/RMB and dragging
                bindm = $mainMod, mouse:272, movewindow
                bindm = $mainMod, mouse:273, resizewindow
                  '';
          };

        };
      };
      gtk_setup = {config, pkgs, ...}: {
        gtk = {
          enable = true;
          theme = {
            package = pkgs.gnome.gnome-themes-extra;
            name = "Adwaita";
          };
          iconTheme = {
            package = pkgs.gnome.adwaita-icon-theme;
            name = "Adwaita";
          };
          cursorTheme = {
            package = pkgs.vanilla-dmz;
            name = "Vanilla-DMZ";
            size = 16;
          };
        };
      };
      gpg-agent-emacs = {config, pkgs, ...}: {
        services = {
          gpg-agent = {
            enable = true;
            defaultCacheTtl = 72000;
            pinentryPackage = pkgs.pinentry-curses; # other interesting flavours emacs tty curses
            extraConfig = ''
                  allow-emacs-pinentry
                  allow-loopback-pinentry
                '';

            maxCacheTtl = 72000;

          };
        };
      };
      emacs-hm-init = {config, pkgs, ...}: {
        home.packages = with pkgs; [
          imagemagickBig
          librsvg
          inputs.pandoc-crossref.packages.x86_64-linux.pandoc-with-crossref
          pdf2svg
          graphviz-nox
        ];
        services = {
          emacs = {
            enable = true;
            defaultEditor = true;
            ## Allow server to start with graphics so wayland session is
            ## correctly detected by server
            # socketActivation.enable = true;
            startWithUserSession = "graphical";
          };
        };
        programs = {
          emacs = {
            enable = true;
            package = pkgs.emacs29-pgtk;
            extraPackages = epkgs: [
            ];
            overrides = final: prev: {
              # org-ref = prev.org-ref.overrideAttrs (old: {
              #   src = pkgs.fetchFromGitHub {
              #     owner = "PhDyellow";
              #     repo = "org-ref";
              #     rev = "11013e702630a6ce2f7fd5da0bc0ac13e53eeea4";
              #     hash = "sha256-friWP2Hyk6aq/+0esnbpI1xr8Z36I3HtXQq5Di0yVzA=";
              #   };
              #   commit = "11013e702630a6ce2f7fd5da0bc0ac13e53eeea4";
              # });
              org-super-links = prev.emacs.pkgs.trivialBuild {
                pname = "org-super-links";
                version = "git";
                src = inputs.org-super-links;
                packageRequires = [
                ];
              };

              org-slt-phdyellow = prev.emacs.pkgs.trivialBuild {
                pname = "org-slt-phdyellow";
                version = "git";
                src = inputs.org-slt-phdyellow;
                packageRequires = [
                  final.org-super-links
                  final.org-sltypes
                  final.transient
                  final.cl-lib
                ];
              };
              org-sltypes = prev.emacs.pkgs.trivialBuild {
                pname = "org-sltypes";
                version = "git";
                src = inputs.org-sltypes;
                packageRequires = [
                  final.org-super-links
                ];
              };
              objed = prev.emacs.pkgs.trivialBuild {
                pname = "objed";
                version = "git";
                src = inputs.objed;
                packageRequires = [
                  final.avy
                  final.key-game
                ];
              };
              key-game = prev.emacs.pkgs.trivialBuild {
                pname = "key-game";
                version = "git";
                src = inputs.key-game;
                packageRequires = [
                ];
              };
              org-linker = prev.emacs.pkgs.trivialBuild {
                pname = "org-linker";
                version = "git";
                src = inputs.org-linker;
                packageRequires = [
                ];
              };
              org-linker-edna = prev.emacs.pkgs.trivialBuild {
                pname = "org-linker-edna";
                version = "git";
                src = inputs.org-linker-edna;
                packageRequires = [
                  final.org-linker
                  final.helm
                ];
              };
              org-transclusion = prev.emacs.pkgs.trivialBuild {
                pname = "org-transclusion";
                version = "git";
                src = inputs.org-transclusion;
                packageRequires = [
                ];
              };
              load-theme-buffer-local = prev.load-theme-buffer-local.overrideAttrs (oldAttrs: {
                src = inputs.color-theme-buffer-local;
              });
              isend-mode = prev.isend-mode.overrideAttrs (oldAttrs: {
                src = inputs.isend-mode;
              });
              smart-tabs-mode = prev.smart-tabs-mode.overrideAttrs (oldAttrs: {
                src = inputs.smart-tabs-mode;
              });
              # denote = prev.denote.overrideAttrs (oldAttrs: {
              # src = inputs.denote;
              # });
              # denote = prev.emacs.pkgs.trivialBuild {
              # pname = "denote";
              # version = "1.2.0";
              # src = inputs.denote;
              # };
            };
            init = {
              enable = true;
              packageQuickstart = true;
              recommendedGcSettings = true;
              startupTimer = true;
              earlyInit = "";
              #home-manager.users.<name> is an attribute set {} of users. Each user is a hmModule, so I can import
              #modules to it. Any modules imported by all users can go in home-manager.sharedModules
              prelude = ''
                      ;;(setq my-user-emacs-directory "/storage/emulated/0/memx/repos/phone_emacs/")

                      (setq my-memx-dir "/para/areas/memx___syncthing/"
                            my-memx-version "memx_v4"
                            my-bib-dir "/para/areas/bibliography___CITE/"
                            my-bib-files `(;"/para/areas/bibliography___CITE/new_refs.bib"
                            ;"/para/areas/bibliography___CITE/new_refs2.org"
                            ;"/para/areas/bibliography___CITE/readings.bib"
                            ;"/para/areas/bibliography___CITE/readings2.org"
                            ;(expand-file-name "new_refs.org" my-memx-dir)
                            ,(expand-file-name "20231027T152659057802-readings___CITE.org" my-memx-dir))
                            my-ereading-dir "/para/areas/bibliography___CITE/ereading___pdf__ebook__refs/"
                            my-html-dir "/para/areas/bibliography___CITE/web-capture___html__org__refs/"
                            my-refs-dirs (list my-ereading-dir my-html-dir)
                      )

                      (setq temporary-file-directory "/para/tmp")


                      (setq make-backup-files nil
                            vc-make-backup-files nil
                            create-lockfiles nil
                            backup-directory-alist `(("." . ,(concat user-emacs-directory
                              ".local/cache/backups")))
                            save-place-file (concat user-emacs-directory ".local/cache/places"))
                      ;;https://vernon-grant.com/emacs/tmux-emacs-and-the-system-clipboard-on-wayland/
                      ;; Checks if the session type is in fact for Wayland.
                      (if (string= (getenv "XDG_SESSION_TYPE") "wayland")
                      ;; credit: yorickvP on Github
                       (let ((wl-copy-process nil))

                          (defun wl-copy (text)
                            (setq wl-copy-process (make-process :name "wl-copy"
                              :buffer nil
                              :command '("wl-copy" "-f" "-n")
                              :connection-type 'pipe))
                            (process-send-string wl-copy-process text)
                            (process-send-eof wl-copy-process))

                        (defun wl-paste ()
                          (if (and wl-copy-process (process-live-p wl-copy-process))
                            nil ; should return nil if we're the current paste owner
                            (shell-command-to-string "wl-paste -n | tr -d \\r")))

                        (setq interprogram-cut-function 'wl-copy)
                        (setq interprogram-paste-function 'wl-paste))
                        ;;else set up x clipboard sharing
                        (setq select-enable-clipboard t)
                        (setq select-enable-primary t)
                           )
                      ;; tramp may not play well with use-package
                      (require 'tramp)
                      (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
                      ;; Add the remote's PATH to tramp's search path (why isn't this the default?)
                      (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
                      (setq tramp-backup-directory-alist `(("." . ,(concat user-emacs-directory ".local/cache/backup/"))))

                    '';
              #config inserted after use-package
              postlude = ''
                      ; Seems to break if called too early
                       (citar-org-roam-mode)

                      ;Always start emacs by showing Goals and tasks
                      ;(org-ql-view  "Progressing Goals")

                      ;; Local Variables:
                      ;; no-byte-compile: nil
                      ;; End:
                    '';

              #Packages configured
              usePackage = {
                ## Startup packages. 'After' needs to flow back to an always-loaded package
                editorconfig = {
                  enable = true;
                  config = ''
                          (editorconfig-mode 1)
                          (setq editorconfig-trim-whitespace-mode 'ws-butler-mode)
                        '';
                };
                envrc = {
                  enable = true;
                  config = ''
                          (envrc-global-mode)
                        '';
                };
                pinentry = {
                  enable = true;
                  after = [ "epg" ];
                  config = ''
                          (pinentry-start)
                        '';
                };
                epg = {
                  enable = true;
                  config = ''
                          (setq epg-pinentry-mode 'loopback)
                          ;; fix for gnupg 2.4.1 causing authinfo edits to hang
                          (fset 'epg-wait-for-status 'ignore)
                        '';
                };
                arc-mode = {
                  enable = true;
                  extraPackages = [ pkgs.p7zip ];
                };
                auth-source = {
                  enable = true;
                  config = ''
                          (setq auth-sources '("/secrets/gpg/.authinfo.gpg"))
                        '';
                };
                dash = {
                  enable = true;
                };
                noflet = {
                  after = ["dash"];
                  enable = true;
                  init = ''
                          (require 'dash) ;;bug in noflet, uses dash without requiring it
                        '';
                };
                load-theme-buffer-local = {
                  after = [ "noflet" "god-mode" ];
                  enable = false;
                  init = ''
                          (require 'noflet) ;; bug in load-theme-buffer-local: uses noflet without requiring it
                        '';
                  config = ''
                        '';
                };
                god-mode = {
                  enable = false;
                  init = ''
                          ;;(setq god-mode-enable-function-key-translation nil)
                        '';
                  config = ''
                          (setq god-exempt-major-modes nil)
                          (setq god-exempt-predicates nil)
                          ;;(god-mode)
                          ;; (require 'load-theme-buffer-local)
                          ;; (add-hook 'god-mode-enabled-hook (lambda () (load-theme-buffer-local 'tango (current-buffer))))
                          ;; (add-hook 'god-mode-disabled-hook (lambda () (load-theme-buffer-local 'zenburn (current-buffer))))
                        '';
                  chords = {
                    "ii" = "god-mode-all";
                  };
                  bindLocal = {
                    god-local-mode-map = {
                      "j" = "god-mode-all";
                      "." = "repeat";
                    };
                  };
                };
                undo-fu-session = {
                  enable = true;
                  config = ''
                          (undo-fu-session-global-mode)
                        '';
                };
                bind-key = {
                  enable = true;
                };
                objed = {
                  after = [ "avy" "expand-region" "magit" ];
                  enable = true;
                  config = ''
                          ;(objed-mode)
                          (add-hook 'ess-r-mode-hook #'objed-local-mode)
                          (add-hook 'nix-mode-hook #'objed-local-mode)
                          (add-hook 'bibtex-mode-hook #'objed-local-mode)
                          (add-hook 'elisp-mode-hook #'objed-local-mode)
                          (add-hook 'sh-mode-hook #'objed-local-mode)
                          (setq objed-disabled-modes '(
                            epa-key-list-mode
                            magit-mode
                            ;org-mode
                          ))


                          ;; rebind switch to buffer with consult
                          (keymap-set objed-op-map "b" #'consult-buffer)
                          ;; Add magit shortcut
                          (keymap-set objed-op-map "g" #'magit-status)


                          ;; Avy objed combinations
                          ;; action for copying/killing object
                          ;; action for throwing object through isend
                          ;; action for teleporting object (kill and yank here)
                          ;; all these actions are supposed to leave me where I started
                          ;; never mind, I prefer to use embark or objed, then use 'l' (lower L) to step back
                          (defun my-objed-isend (beg end pref)
                            "Send object to associated buffer with isend"
                            (interactive "r\np")
                            (require 'isend-mode)
                            (if (not isend-mode)
                              (call-interactively #'isend-associate))
                            (isend--send-dest (filter-buffer-substring beg end) (get-buffer isend--command-buffer)))

                          (objed-define-op nil my-objed-isend)
                          ;;objed-define-op will return objed-<my function name>, and I bind the returned function
                          (keymap-set objed-op-map "RET" #'objed-my-objed-isend)

                          (keymap-set objed-op-map "z" #'embark-act)
                          (keymap-set objed-op-map "Z" #'embark-export)
                          (keymap-set objed-op-map "l" #'consult-line)
                        '';
                };
                objed-game = {
                  after = ["objed"];
                  enable = true;
                };
                expand-region = {
                  enable = true;
                };
                avy = {
                  after = [ "embark" ];
                  enable = true;
                  config = ''
                          (defun avy-action-embark (pt)
                            (unwind-protect
                              (save-excursion
                                (goto-char pt)
                                (embark-act))
                              (select-window (cdr (ring-ref avy-ring 0))))
                            t)
                          (setf (alist-get ?o avy-dispatch-alist) 'avy-action-embark)
                        '';
                };
                embark = {
                  enable = true;
                  demand = true; # bind will prevent loading otherwise
                  bind = {
                    "C-h B" = "embark-bindings";
                    "M-o" = "embark-act";
                    "M-O" = "embark-export";
                    "C-;" = "embark-dwim";
                  };
                  config = ''
                          ;; Hide the mode line of the Embark live/completions buffers
                          (add-to-list 'display-buffer-alist
                          '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                          nil
                          (window-parameters (mode-line-format . none))))

                        '';
                };
                consult-recoll = {
                  enable = true;
                  after = [ "consult" "embark" ];
                  config = ''
                        (consult-recoll-embark-setup)
                        (setq consult-recoll-inline-snippets t)
                      '';
                  # extraPackages = [ pkgs.recoll ]; # handling in home-manager
                };
                embark-consult = {
                  after = [ "embark" "consult" ];
                  enable = true;
                  hook = [
                    "(embark-collect-mode . consult-preview-at-point-mode)"
                  ];
                };
                avy-embark-collect = {
                  after = [ "avy" "embark" ];
                  enable = true;
                };
                ace-window = {
                  after = ["avy"];
                  enable = true;
                  config = ''
                          (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                                aw-dispatch-always t)
                          (keymap-set objed-op-map "o" #'ace-window)
                        '';
                  bind = {
                    "C-x o" = "ace-window";
                  };
                };
                origami = {
                  enable = true;
                  config = ''
                          (add-to-list 'origami-parser-alist '(ess-r-mode . origami-c-style-parser))
                          (global-origami-mode)
                        '';
                };
                files = {
                  enable = true;
                  config = ''
                          (setq auto-save-default t
                                auto-save-include-big-deletions t
                                auto-save-list-file-prefix (concat user-emacs-directory ".local/cache/autosave/")
                                auto-save-file-name-transforms (list (list ".*" auto-save-list-file-prefix t))
                                safe-local-variable-values '((magit-wip-merge-branch . t))
                                ;safe-local-variable-directories `(,my-memx-dir)
                                revert-without-query '(".pdf")
                          )
                        '';
                };
                tramp = {
                  enable = false;
                  config = ''
                          (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
                          ;; Add the remote's PATH to tramp's search path (why isn't this the default?)
                          (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
                          (setq tramp-backup-directory-alist `(("." . ,(concat user-emacs-directory ".local/cache/backup/"))))
                        '';
                };
                mouse = {
                  enable = true;
                  config =  ''
                          (setq mouse-yank-at-point t)
                        '';
                };
                apropos = {
                  enable = true;
                  config =  ''
                          (setq apropos-do-all t)
                        '';
                };

                menu-bar = {
                  enable = true;
                  config = ''
                          (menu-bar-mode -1)
                        '';
                };
                tool-bar = {
                  enable = true;
                  config = ''
                          (when (fboundp 'tool-bar-mode)
                          (tool-bar-mode -1))
                        '';
                };
                scroll-bar = {
                  enable = true;
                  config = ''
                          (when (fboundp 'scroll-bar-mode)
                          (scroll-bar-mode -1))
                        '';
                };
                uniquify = {
                  enable = true;
                  config = ''
                          (setq uniquify-buffer-name-style 'forward)
                        '';
                };
                ## May replace load-buffer-local-theme
                prism = {
                  enable = true;
                  config = ''
                          ;;prism-colors was generated by
                          ;;running
                          ;;(prism-set-colors :num 24
                          ;;:colors  (zenburn-with-color-variables
                          ;;(list zenburn-red zenburn-green zenburn-orange zenburn-blue zenburn-yellow zenburn-magenta))
                          ;;:lightens (list 0 5 10 20)
                          ;;:desaturations (list 0 00 0 00  ))
                          ;;))
                          ;;(setq custom-file "~/emacs-custom-hack.el")
                          ;;(prism-save-colors)
                          ;; then reading the value of prism-colors from "~/emacs-custom-hack.el"

                          (setq
                            prism-colors '("#cc9393" "#7f9f7f" "#dfaf8f" "#8cd0d3" "#f0dfaf" "#dc8cc3" "#d19e9e" "#87a587" "#e3b99d" "#98d5d7" "#f3e5c0" "#e099ca" "#d7aaaa" "#8fab8f" "#e7c3ab" "#a5dadc" "#f6ecd1" "#e4a7d1" "#e2c2c2" "#9fb79f" "#efd7c7" "#bee4e6" "#fdfaf4" "#ecc3df")
                            prism-desaturations '(0)
                            prism-lightens '(0)
                            prism-num-faces 24
                            )
                          (prism-set-colors)
                        '';
                };
                smartparens = {
                  enable = true;
                };
                smartparens-config = {
                  after = [ "smartparens" ];
                  enable = true;
                  config = ''
                          ;; Turn off smartparens auto features,
                          ;; Sometimes they don't hurt me,
                          ;; But other times I have to fight them
                          ;; I'm never glad the closing bracket has
                          ;; been inserted for me
                          ;; (setq sp-autowrap-region nil
                          ;;      sp-autodelete-pair nil
                          ;;      sp-autodelete-opening-pair nil
                          ;;      sp-autodelete-closing-pair nil
                          ;;      sp-autoinsert-pair nil
                          ;;      sp-autodelete-wrap nil
                          ;;      sp-autoskip-opening-pair nil
                          ;;      sp-autoskip-closing-pair nil
                          ;;      sp-escape-quotes-after-insert nil)
                          ;; I only want show-smartparens
                          ;; (smartparens-global-mode)
                          (show-smartparens-global-mode)
                        '';
                };
                vundo = {
                  enable = true;
                };
                frame = {
                  enable = true;
                  config = ''
                          (setq default-frame-alist '((font . "FiraCode Nerd Font 18")))
                          (blink-cursor-mode 0)
                        '';
                };
                crm = {
                  enable = true;
                  config = ''
                          (defun crm-indicator (args)
                            (cons (format "[CRM%s] %s"
                              (replace-regexp-in-string
                                "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                                crm-separator)
                              (car args))
                              (cdr args)))
                          (advice-add #'completing-read-multiple :filter-args #'crm-indicator)


                          (setq read-extended-command-predicate
                          #'command-completion-default-include-p)
                        '';
                };
                simple = {
                  enable = true;
                  config = ''
                          (setq read-extended-command-predicate
                                  #'command-completion-default-include-p
                                save-interprogram-paste-before-kill t
                          )
                          (setq-default indent-tabs-mode -1)
                        '';
                };
                emacs = {
                  enable = true;
                  config = ''
                          (setq enable-recursive-minibuffers t)

                          (setq ring-bell-function
                                (lambda ()
                                  (let ((orig-fg (face-foreground 'mode-line)))
                                    (set-face-foreground 'mode-line "#F2804F")
                                    (run-with-idle-timer 0.1 nil
                                      (lambda (fg) (set-face-foreground 'mode-line fg))
                                      orig-fg))))

                           (setq completion-cycle-threshold 3)

                           (setq sentence-end-double-space nil)

                        '';
                };
                isend-mode = {
                  enable = true;
                };
                agenix = {
                  enable = false;
                  extraPackages = [ pkgs.rage pkgs.pinentry-emacs ];
                  config = ''
                        (setq agenix-age-program "rage"
                           agenix-key-files
                             '(
                               "/home/phil/.ssh/id_phil_prime_ai_nixos_ed25519"
                             ))
                      '';
                };
                age = {
                  enable = false;
                  extraPackages = [ pkgs.rage pkgs.pinentry-emacs ];
                  config = ''
                        (age-file-enable)
                      '';
                  extraConfig = ''
                        :custom (age-program "rage")
                           (age-default-identity `(,(expand-file-name "~/.ssh/id_phil_prime_ai_nixos_ed25519")))
                           (age-default-recipient `(,(expand-file-name "~/.ssh/id_phil_prime_ai_nixos_ed25519.pub")))
                          (age-always-use-default-keys nil)
                          (age-pinentry-mode 'ask)
                      '';
                };
                magit = {
                  enable = true;
                  config = ''
                          (setq magit-refresh-status-buffer nil)
                        '';
                };
                orgit = {
                  enable = true;
                };
                forge = {
                  enable = true;
                };
                git-timemachine = {
                  enable = true;
                };
                git-gutter = {
                  enable = true;
                  config = ''
                    (setq git-gutter:update-interval 5)
                    ;(set-face-foreground 'git-gutter:added "#00FF00")
                  '';
                  hook = [
                    "(prog-mode . git-gutter-mode)"
                    "(org-mode . git-gutter-mode)"
                  ];
                  # extraConfig = ''
                  #       :custom
                  #         ;(git-gutter:window-width 2)
                  #         ;(git-gutter:modified-sign (nerd-icons-octicon "nf-oct-diff_modified"))
                  #         ;(git-gutter:added-sign (nerd-icons-octicon "nf-oct-diff_added"))
                  #         ;(git-gutter:deleted-sign (nerd-icons-octicon "nf-oct-diff_removed"))
                  # '';
                };
                git-gutter-fringe = {
                  enable = false;
                  config = ''
                    (define-fringe-bitmap 'git-gutter-fr:added [#b11100000] nil nil '(center repeated))
                    (define-fringe-bitmap 'git-gutter-fr:modified [#b11100000] nil nil '(center repeated))
                    (define-fringe-bitmap 'git-gutter-fr:added [#b10000000
                                                                #b11000000
                                                                #b11100000
                                                                #b11110000] nil nil 'bottom)


                  '';
                };
                vterm = {
                  enable = true;
                };
                eat = {
                  enable = true;
                };
                xref = {
                  enable = true;
                };
                consult-xref = {
                  enable = true;
                  command = [ "consult-xref" ];
                  after = [ "consult" "xref" ];
                  init = ''
                          ;; Use Consult to select xref locations with preview
                          (setq xref-show-xrefs-function #'consult-xref
                          xref-show-definitions-function #'consult-xref)
                      '';
                };
                consult = {
                  enable = true;
                  command = [ "consult-xref" ];
                  hook = [
                    "(completion-list-mode . consult-preview-at-point-mode)"
                  ];
                  init = ''
                          ;; Optionally configure the register formatting. This improves the register
                          ;; preview for `consult-register', `consult-register-load',
                          ;; `consult-register-store' and the Emacs built-ins.
                          (setq register-preview-delay 0.5
                          register-preview-function #'consult-register-format)

                          ;; Optionally tweak the register preview window.
                          ;; This adds thin lines, sorting and hides the mode line of the window.
                          (advice-add #'register-preview :override #'consult-register-window)

                        '';
                  config = ''
                          ;; Optionally configure preview. The default value
                          ;; is 'any, such that any key triggers the preview.
                          ;; (setq consult-preview-key 'any)
                          ;; (setq consult-preview-key (kbd "M-."))
                          ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
                          ;; For some commands and buffer sources it is useful to configure the
                          ;; :preview-key on a per-command basis using the `consult-customize' macro.
                          (consult-customize
                          consult-theme :preview-key '(:debounce 0.2 any)
                          consult-ripgrep consult-git-grep consult-grep
                          consult-bookmark consult-recent-file consult-xref
                          consult--source-bookmark consult--source-file-register
                          consult--source-recent-file consult--source-project-recent-file
                          ;; :preview-key (kbd "M-.")
                          :preview-key '(:debounce 0.4 any))

                          ;; Optionally configure the narrowing key.
                          ;; Both < and C-+ work reasonably well.
                          (setq consult-narrow-key "<") ;; (kbd "C-+")
                        '';
                  bind = {
                    "C-x C-b" = "consult-buffer";
                    "C-x C-y" = "consult-yank-from-kill-ring";
                    "C-x j b" = "consult-buffer";
                    "C-x j l" = "consult-line";
                    "C-x j y" = "consult-yank-from-kill-ring";
                  };
                };
                consult-dir = {
                  enable = true;
                };
                consult-flycheck = {
                  enable = true;
                  command = [ "consult-flycheck"];
                };
                flycheck = {
                  enable = true;
                  config = ''
                        (global-flycheck-mode)
                        (setq flycheck-check-syntax-automatically '(save idle-change)
                              flycheck-idle-change-delay 5)
                      '';
                };
                consult-org = {
                  enable = true;
                  command = [ "consult-org" ];
                };
                marginalia = {
                  enable = true;
                  demand = true;
                  config = ''
                          (marginalia-mode)
                        '';
                  bindLocal = {
                    minibuffer-local-map = {
                      "M-A" = "marginalia-cycle";
                    };
                  };
                };
                orderless = {
                  enable = true;
                  init = ''
                          (setq completion-styles '(orderless basic)
                          completion-category-defaults nil
                          completion-category-overrides '((file (styles partial-completion))))
                        '';
                };
                autorevert = {
                  enable = true;
                  config = ''
                          (setq global-auto-revert-non-file-buffers t)
                          (global-auto-revert-mode 1)
                        '';
                };
                saveplace = {
                  enable = true;
                  config = ''
                          (save-place-mode 1)
                        '';
                };
                recentf = {
                  enable = true;
                  init = ''
                          (setq recentf-max-saved-items nil
                                recentf-save-file (concat user-emacs-directory ".local/cache/recentf"))
                          (recentf-mode 1)
                        '';
                };
                savehist = {
                  enable = true;
                  config = ''
                          (setq savehist-additional-variables
                                '(search-ring
                                  regexp-search-ring
                                  mark-ring
                                  global-mark-ring))
                          (setq history-length 250)
                          (savehist-mode)
                        '';
                };
                vertico = {
                  enable = true;
                  config = ''
                          (vertico-mode)
                        '';
                };
                vertico-quick = {
                  after = [ "vertico" ];
                  enable = true;
                  bindLocal = {
                    vertico-map = {
                      "M-q" = "vertico-quick-insert";
                      "C-q" = "vertico-quick-exit";
                    };
                  };
                };
                vertico-buffer = {
                  after = [ "vertico" ];
                  enable = false;
                  config = ''
                          (vertico-buffer-mode)
                          (setq vertico-buffer-display-action
                          '(display-buffer-in-side-window (side . left)
                          (window-width . 0.5)))
                        '';
                };
                vertico-directory = {
                  after = [ "vertico" ];
                  enable = true;
                  bindLocal = {
                    vertico-map = {
                      "RET" = "vertico-directory-enter";
                      "DEL" = "vertico-directory-delete-char";
                      "M-DEL" = "vertico-directory-delete-word";
                    };
                  };
                  hook = [
                    "(rfn-eshadow-update-overlay . vertico-directory-tidy)"
                  ];
                };
                image-dired = {
                  enable = true;
                  config = ''
                          (setq image-dired-thumbnail-storage 'standard-large)
                        '';
                };
                one-themes = {
                  enable = true;
                };
                nerd-icons = {
                  enable = true;
                  config = ''
                          (setq nerd-icons-font-family "FiraCode Nerd Font")
                        '';
                };
                kind-icon = {
                  enable = true;
                  after = [ "corfu" "nerd-icons" ];
                  config = ''
                          (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
                          (setq kind-icon-use-icons nil)
                          (setq kind-icon-mapping
                            `(
                                (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
                                (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
                                (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
                                (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
                                (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
                                (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
                                (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
                                (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
                                (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
                                (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
                                (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
                                (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
                                (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
                                (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
                                (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
                                (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
                                (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
                                (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
                                (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
                                (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
                                (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
                                (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
                                (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
                                (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
                                (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
                                (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
                                (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
                                (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
                                (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
                                (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
                                (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
                                (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
                                (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
                                (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
                                (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
                                (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))

                                ;(plist-put kind-icon-default-style :height 0.9)

                          ;(setq kind-icon-default-face 'corfu-default)
                        '';
                };
                corfu = {
                  enable = true;
                  command = [ "global-corfu-mode" ];
                  init = ''
                          (global-corfu-mode)
                        '';
                  config = ''
                          (setq corfu-quit-no-match nil
                                corfu-quit-at-boundary 'separator
                                corfu-preview-current nil
                                corfu-preselect 'prompt
                                corfu-scroll-margin 5
                          )

                          (defun corfu-enable-always-in-minibuffer ()
                            "Enable Corfu in the minibuffer if Vertico/Mct are not active."
                            (unless (or (bound-and-true-p mct--active)
                                        (bound-and-true-p vertico--input)
                                        (eq (current-local-map) read-passwd-map))
                              ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
                              (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                                          corfu-popupinfo-delay nil)
                              (corfu-mode 1)))
                          (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

                          (add-hook 'eshell-mode-hook
                            (lambda ()
                              (setq-local corfu-auto nil)
                              (corfu-mode)))

                          (defun corfu-send-shell (&rest _)
                            "Send completion candidate when inside comint/eshell."
                            (cond
                             ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
                              (eshell-send-input))
                             ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
                              (comint-send-input))))

                          (advice-add #'corfu-insert :after #'corfu-send-shell)

                          (defun corfu-move-to-minibuffer ()
                            (interactive)
                            (when completion-in-region--data
                              (let ((completion-extra-properties corfu--extra)
                                    completion-cycle-threshold completion-cycling)
                                (apply #'consult-completion-in-region completion-in-region--data))))
                          (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)

                          (keymap-set corfu-map "M-q" #'corfu-quick-complete)
                          (keymap-set corfu-map "C-q" #'corfu-quick-insert)


                        '';
                };
                corfu-popupinfo = {
                  enable = true;
                  after = [ "corfu" ];
                  config = ''
                          (corfu-popupinfo-mode 1)
                      '';
                };
                cape = {
                  enable = true;
                  after = [ "corfu" ];
                  init = ''
                          ;;Add `completion-at-point-functions' used by `completion-at-point'.
                          ;; Order matters
                          (add-to-list 'completion-at-point-functions #'cape-dabbrev)
                          (add-to-list 'completion-at-point-functions #'cape-file)
                          (add-to-list 'completion-at-point-functions #'cape-elisp-block)
                        '';
                  bind = {
                    "M-n f" = "cape-file";
                    "M-n d" = "cape-dabbrev";
                    "M-n e" = "cape-elisp-block";
                  };
                };
                zenburn-theme = {
                  enable = true;
                  init = ''
                          (setq zenburn-use-variable-pitch t)
                        '';
                  config = ''
                          (load-theme 'zenburn t)
                        '';
                };
                browse-url = {
                  enable = true;
                  config = ''
                          (setq browse-url-generic-program "nyxt"
                                browse-url-browser-function 'eww-browse-url
                                browse-url-secondary-browser-function 'browse-url-generic)
                        '';
                };
                which-key = {
                  enable = true;
                  config = ''
                          (which-key-mode)
                        '';
                };
                org = {
                  enable = true;
                  demand = true;
                  init = ''
                        '';
                  config = ''
                          ;;Add R to org-babel
                          (add-to-list 'org-babel-load-languages (cons (intern "R") t))
                          (org-babel-do-load-languages
                            'org-babel-load-languages
                            org-babel-load-languages)


                          (setq org-agenda-files `(,my-memx-dir)
                                org-directory my-memx-dir
                          ;      org-agenda-file-regexp "\\`[^.].*_agenda.*\\.org\\'"
                          )

`                         ;; Use svg for latex preview
                          (setq org-preview-latex-default-process dvisvgm)

                          ;;Allow code blocks to execute without asking me every time
                          ;; for safetly though, don't allow C-c C-c to evaluate blocks
                           (setq org-confirm-babel-evaluate (lambda (lang src) (if (string= lang "R") nil t))
                                 org-babel-no-eval-on-ctrl-c-ctrl-c t)

                          ;;never run blocks on export. Creates consisent results for R async session blocks.
                          (add-to-list 'org-babel-default-header-args '(:eval . "never-export"))

                          (setq org-enforce-todo-dependencies t)

                          ;;set up todo entries
                          (setq org-todo-keywords '(
                            (sequence "QUERY(y)" "|" "RESOLVED(!)") ;A question
                            (sequence "GOAL(g)" "|" "ACHIEVED(!)") ;An outcome I want to see come about, ideally with a deadline
                            ;(sequence "PROJ(p)" "|" "COMPLETE(!)") ;A set of tasks supporting a goal or higher project
                            (sequence "TASK(t)" "|" "DONE(!)") ; A task. Tasks should pursue goals. A group of related tasks are really just subtasks, no projects.
                            (sequence "SPARK(s)" "THINKING(n)" "|" "PROCESSED(!)") ;Transient notes that I want to capture
                            (sequence "READ(r)" "|" "CITE(c)") ;bibliographic entries. Switch back and forth as needed
                            (sequence "|" "CONCEPT(w)" "VIEW(v)") ;evergreen concept note or overview of area
                            ;(sequence "PROBLEM" "|" "SOLVED" "SKIP")
                            ;(sequence "ATTEMPT" "|" "FAIL" "SUCCESS")
                            ;(sequence "BLOCKED(b)" "TODO(t)" "NEXT(n!)" "WAIT(b@/!)" "IDEA(i@)" "|" "NEVER(x@)" "DONE(d!)")
                            ;(sequence "ADD" "FIND" "SKIM(1!)" "1ST_READ(2!)" "1ST_MARG(3!)" "2ND_READ(4!)" "2ND_MARG(5!)" "3RD_READ(6!)" "3RD_MARG(7!)" "GOLD(8!)" "|" "REF(9!)" "DROP(k)")
                          ))

                          (setq org-tag-alist '(
                          ;; (startgroup . nil) ... (endgroup . nil) -> Mutually exclusive  tags
                          ;; (startgroup . nil) (tagggx . "t") (grouptags . nil) ... (endgroup . nil) -> Mutually exclusive tags in tag group "tagggx"
                          ;; (startgrouptag . nil) (taggg . "t") (grouptags . nil) ... (endgrouptag . nil) ->  tag group "taggg", not exclusive
                          ;; regular tag "proj"
                          ;; regexp tag "{proj_.+}" -> matches any tag starting with "proj_". Useful as part of group tag, eg ((startgroup) ("proj") (grouptags) ("{proj_.+}")(endgroup)) which allows searches for "proj" to match all projects, or proj_win to just match the proj_win project.

                          ;; If a tag is mutually exclusive with PROCESSING, then the tag is always a processing tag.

                            ;; QUERY tag for question, investigation log, conclusion.
                            (startgroup . nil) ("QUERY") ("PROCESSING") ("XXQUERY") (endgroup . nil)
                            ;; RECIPE tag for how to do something
                            (startgroup . nil) ("RECIPE") ("XXRECIPE") (endgroup . nil)
                            ;; CITE tag for a reference or source. XXCITE is a retracted source
                            (startgroup . nil) ("CITE") ("XXCITE") (endgroup . nil)
                            ;; SPARK tag for an idea, probably crossing together a lot of other ideas and sources. XXSPARK is a spark that has been processed
                            (startgroup . nil) ("SPARK") ("PROCESSING") ("XXSPARK") (endgroup . nil)
                            ;; PROJ tag for a goal with tasks. Use PROJ if it is something that can be marked as done, is more complex than a single task and achieves something in itself when done (ie. not just partway to a desired state. A counterexample: submitting an application then accepting the returned offer are two tasks, the first one is not a complete project on it's own.). A subproject is also a PROJ, linked to the parent proj in a super/sub type link pair.
                            (startgroup . nil) ("PROJ") ("XXPROJ") (endgroup . nil)
                            ;; TASK tag for a task. Often redundant due to TODO states.
                            (startgroup . nil) ("TASK") ("XXTASK") (endgroup . nil)
                            ;; AREA tag for things I am responsible for, and want to maintain, but cannot be marked as done. I'm never really "done" with being a parent or husband, for example, or taking care of my health. Areas do close though, as life circumstances change.
                            (startgroup . nil) ("AREA") ("XXAREA") (endgroup . nil)
                            ;; VALUE tag for my fundamental understanding of things that are important. Values are concepts. Values are used to discern between actions that all seem good.
                            (startgroup . nil) ("VALUE") ("XXVALUE") (endgroup . nil)
                            ;; GOAL tag for things I want to work towards. Goals are visions of the future. An active goal is defined by a clear outcome. Goals pursue values, values inform goals.
                            (startgroup . nil) ("GOAL") ("XXGOAL") (endgroup . nil)
                            ;; WORKS tag for things I am working on. Code, writing, in collaboration, solo... They are marked as closed when I don't plan on changing anything else in them.
                            (startgroup . nil) ("WORKS") ("XXWORKS") (endgroup . nil)
                            ;; CONCEPT tag for notes that explain a concept. Theseare the "evergreen" notes in my memx, and should be reasonably self-contained and understandable to an outsider. Concept notes are not normally closed, but they might be retracted.
                            (startgroup . nil) ("CONCEPT") ("XXCONCEPT") (endgroup . nil)
                            ;; ENTITY  tag, for a thing, like a person, company, city or software program. Entity tags are closed when I no longer want to see them, not when the entity ceases to exist, as some entities are historical or fictional.
                            (startgroup . nil) ("ENTITY") ("XXENTITY") (endgroup . nil)
                            ;; Contains TOC, org-ql-views, overview, or some kind of summary that mostly just links to other things.
                            (startgroup . nil) ("VIEW") ("XXVIEW") (endgroup . nil)
                            ;; THINKING tag, for thinking things through. Like SPARK, but when I dont have an idea.
                            (startgroup . nil) ("THINKING") ("PROCESSING") ("XXTHINKING") (endgroup . nil)


                          ))

                          (setq org-M-RET-may-split-line nil)

                          (setq org-log-into-drawer t
                                org-log-redeadline t
                                org-log-reschedule t
                                org-log-done t)

                          (setq org-image-actual-width '(800))

                          (setq org-bibtex-tags '("CITE"))

                          ;; TODO org-after-tags-change-hook in memx to rename file to match tags. filetags could be quite useful here if I set it to match my tag

                        '';
                };
                ox-pandoc = {
                  after = [ "org" ] ;
                  enable = true;
                  config = ''
                        (setq org-pandoc-options '(
                          (standalone  . t)
                          (number-sections . t)
                        )
                         org-pandoc-format-extensions '(docx+native_numbering)
                        )


                      '';
                };
                ox-latex = {
                  after = [ "org" ];
                  enable = true;
                  config = ''
(setq org-latex-prefer-user-labels t)

;; Override some ox-latex functions to support xltabular
(defun org-latex--org-table (table contents info)
  "Return appropriate LaTeX code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' property and
`table' as its `:mode' attribute."
  (let* ((attr (org-export-read-attribute :attr_latex table))
	 (alignment (org-latex--align-string table info))
         (opt (org-export-read-attribute :attr_latex table :options))
	 (table-env (or (plist-get attr :environment)
			(plist-get info :latex-default-table-environment)))
	 (width
	  (let ((w (plist-get attr :width)))
	    (cond ((not w) "")
		  ((member table-env '("tabular" "longtable")) "")
		  ((member table-env '("tabu" "longtabu"))
		   (format (if (plist-get attr :spread) " spread %s "
			     " to %s ")
			   w))
		  (t (format "{%s}" w)))))
	 (caption (org-latex--caption/label-string table info))
	 (above? (org-latex--caption-above-p table info)))
    (cond
     ((member table-env '("longtable" "longtabu" "xltabular"))
      (let ((fontsize (let ((font (plist-get attr :font)))
			(and font (concat font "\n")))))
	(concat (and fontsize (concat "{" fontsize))
		(format "\\begin{%s}%s{%s}\n" table-env width alignment)
		(and above?
		     (org-string-nw-p caption)
		     (concat caption org-latex-line-break-safe "\n"))
		contents
		(and (not above?)
		     (org-string-nw-p caption)
		     (concat caption org-latex-line-break-safe "\n"))
		(format "\\end{%s}" table-env)
		(and fontsize "}"))))
     (t
      (let ((output (format "\\begin{%s}%s%s{%s}\n%s\\end{%s}"
			    table-env
                            (if opt (format "[%s]" opt) "")
			    width
			    alignment
			    contents
			    table-env)))
	(org-latex--decorate-table output attr caption above? info))))))

(defun org-latex-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to LaTeX.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  (let* ((attr (org-export-read-attribute :attr_latex
					  (org-export-get-parent table-row)))
	 (booktabsp (if (plist-member attr :booktabs) (plist-get attr :booktabs)
		      (plist-get info :latex-tables-booktabs)))
	 (longtablep
	  (member (or (plist-get attr :environment)
		      (plist-get info :latex-default-table-environment))
		  '("longtable" "xltabular" "longtabu"))))
    (if (eq (org-element-property :type table-row) 'rule)
	(cond
	 ((not booktabsp) "\\hline")
	 ((not (org-export-get-previous-element table-row info)) "\\toprule")
	 ((not (org-export-get-next-element table-row info)) "\\bottomrule")
	 ((and longtablep
	       (org-export-table-row-ends-header-p
		(org-export-get-previous-element table-row info) info))
	  "")
	 (t "\\midrule"))
      (concat
       ;; When BOOKTABS are activated enforce top-rule even when no
       ;; hline was specifically marked.
       (and booktabsp (not (org-export-get-previous-element table-row info))
	    "\\toprule\n")
       contents org-latex-line-break-safe "\n"
       (cond
	;; Special case for long tables.  Define header and footers.
	((and longtablep (org-export-table-row-ends-header-p table-row info))
	 (let ((columns (cdr (org-export-table-dimensions
			      (org-export-get-parent-table table-row) info))))
	   (format "%s
\\endfirsthead
\\multicolumn{%d}{l}{%s} \\\\[0pt]
%s
%s \\\\[0pt]\n
%s
\\endhead
%s\\multicolumn{%d}{r}{%s} \\\\
\\endfoot
\\endlastfoot"
		   (if booktabsp "\\midrule" "\\hline")
		   columns
		   (org-latex--translate "Continued from previous page" info)
		   (cond
		    ((not (org-export-table-row-starts-header-p table-row info))
		     "")
		    (booktabsp "\\toprule\n")
		    (t "\\hline\n"))
		   contents
		   (if booktabsp "\\midrule" "\\hline")
		   (if booktabsp "\\midrule" "\\hline")
		   columns
		   (org-latex--translate "Continued on next page" info))))
	;; When BOOKTABS are activated enforce bottom rule even when
	;; no hline was specifically marked.
	((and booktabsp (not (org-export-get-next-element table-row info)))
	 "\\bottomrule"))))))


                      '';
                };
                ox-html = {
                  after = [ "org" ];
                  enable = true;
                  config = ''
                        (setq org-html-prefer-user-labels t)
                      '';
                };
                org-clock = {
                  after = [ "org" ];
                  enable = true;
                  config = ''
                          (setq
                                ;;enable auto-clock resolution for finding open clocks
                                org-clock-auto-clock-resolution 'when-no-clock-is-running
                                org-clock-report-include-clocking-task t ;; include current clocking task in clock reports
                                ;; save the running clock and all clock history when exiting emacs, and load it on startup
                          org-clock-persist t
                          ;; resume clocking task on clock-in if the clock is open
                          org-clock-in-resume t
                          ;;automatically start clock from last clock out
                          org-clock-continuously nil
                          )
                        '';
                };
                org-ql = {
                  enable = true;
                  after = [ "org" ];
                  config = ''

                      (defun my-org-agenda-blocked-p (item)
                             "Returns t if the org-agenda item is blocked"
                             (let* ((marker (get-text-property 0 'org-marker item))
																        (buffer (marker-buffer marker)))
																  (save-excursion
																	 (switch-to-buffer-other-window buffer)
																	 (goto-char marker)
																	 (org-entry-blocked-p))))
                        (setq org-ql-views '())
                        (add-to-list 'org-ql-views
                          `("Clean memx"
                            :buffers-files org-agenda-files
                            :query (or (todo)
                            (done)
                            (not (todo)))
                            :narrow nil
                            :super-groups (
                                          (:name "Open Queries"
                                          :todo "QUERY")
                                          (:name "Open Sparks"
                                          :todo ("SPARK" "THINKING"))
                                          (:name "Old Format"
                                          :not (:tag ,my-memx-version))
                                          (
                                          :discard (:anything t))
                            )))
                        (add-to-list 'org-ql-views
                          '("Cold Entries"
                            :buffers-files org-agenda-files
                            :query (closed :to -14)
                            :narrow nil
                            :super-groups (
                              (:auto-ts)
                            )))
                        (add-to-list 'org-ql-views
                          '("Citations to Read"
                          :buffers-files org-agenda-files
                          :query (todo "READ")
                          :narrow nil))
                        (add-to-list 'org-ql-views

                          '("Progressing Goals"
                          :buffers-files org-agenda-files
                          :query (or
																		 (todo "GOAL" "TASK" "NEXT")
																		(tags "VALUE"))
	                        :narrow nil
                          :super-groups (
                            (:name "Values"
												     :tag "VALUE")
                            (:name "Dangling Goals (No tasks blocking Goal)"
                             :and (:todo "GOAL"
                                   :not (:pred (lambda (item)
																 (my-org-agenda-blocked-p item)))))
									          (:todo "GOAL"
												     :name "Active Goals")
                            (:name "Blocked Deadlines"
                             :and (:pred (lambda (item)
																 (my-org-agenda-blocked-p item))
                                 :deadline t))
                            (:name "Deadlines"
                             :deadline t)
                            (:name "Blocked Scheduled"
                             :and (:pred (lambda (item)
																 (my-org-agenda-blocked-p item))
                                 :scheduled t))
                            (:name "Scheduled"
                             :scheduled t)
                            (:name "Next Task (Block some if more than one)"
                             :not (:pred (lambda (item)
																 (my-org-agenda-blocked-p item))))
                            (:name "Blocked Tasks"
                             :pred (lambda (item)
																 (my-org-agenda-blocked-p item))))))
'';
                };
                org-sidebar = {
                  enable = true;
                  config = ''
                        (defun my-org-sidebar ()
  "Display my Org Sidebar."
  (interactive)
  (org-sidebar
   :sidebars (make-org-sidebar
              :name "My Sidebar"
              :description "My sidebar items"
              :items (org-ql (org-agenda-files)
                       (and (not (done))
                            (or (deadline auto)
                                (scheduled :on today)))
                       :action element-with-markers))))
                      '';
                };
                #org link library
                ol = {
                  enable = true;
                  after = [ "org" ];
                  config = ''
                          (setq org-link-abbrev-alist
                          '(("websearch"      . "https://html.duckduckgo.com/html/?q=%s")
                             ("gscholar" . "https://scholar.google.com/scholar?q=%s")))
                        '';
                };
                helm = {
                  enable = true;
                };
                helm-org-ql = {
                  enable = true;
                };
                helm-org-rifle = {
                  enable = true;
                };
                org-super-links = {
                  enable = true;
                  after = [ "org" ];
                  config = ''
                          (setq org-super-links-search-function "helm-org-rifle")
                        '';
                };
                org-sltypes = {
                  after = [ "org-super-links" ];
                  enable = true;
                };
                org-slt-phdyellow = {
                  after = ["org-sltypes"];
                  command = [ "org-slt-phdyellow" ];
                  enable = true;
                  bindLocal = {
                    org-mode-map = {
                      "C-c C-i" = "org-slt-phdyellow";
                    };
                  };
                };
                org-transclusion = {
                  after = [ "org" "zenburn-theme" ];
                  enable = true;
                  config = ''

                        (add-to-list 'org-transclusion-extensions 'org-transclusion-src-lines)
                        (require 'org-transclusion-src-lines)

                        (add-to-list 'org-transclusion-extensions 'org-transclusion-font-lock)
                        (require 'org-transclusion-font-lock)

                        (require 'zenburn-theme)
                        (set-face-attribute 'org-transclusion-fringe nil
                           :background "red"
                           :foreground "red"
                        )
                        (set-face-attribute 'org-transclusion-source-fringe nil
                           :background "coral"
                           :foreground "coral"
                        )

                        (set-face-attribute 'org-transclusion nil
                            :background (cdr (assoc "zenburn-blue-5" zenburn-default-colors-alist)))
                        (set-face-attribute 'org-transclusion-source nil
                            :background (cdr (assoc "zenburn-green-5" zenburn-default-colors-alist)))
                      '';
                };
                org-edna = {
                  after = [ "org" ];
                  enable = true;
                  config = ''
                          (org-edna-mode)

;;; trc-workgraph.el --- Visualize org mode files as graphs  -*- lexical-binding: t; -*- <jacek@zlydach.pl>

;; Copyright (C) 2021  Jacek "TeMPOraL" Złydach

;; Author: Jacek "TeMPOraL" Złydach <jacek@zlydach.pl>
;; Keywords: org-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org-element)
(require 's)



;;; Current schema:
;;; Graph: (connectome nodelist)
;;; Connectome: # s{id -> ((id, relation-type)...)}
;;; Relation-type: finish-to-finish | start-to-finish
;;; Nodelist: (node...)
;;; Node: (id title todo-state tags first-sentence [date-start date-complete date-deadline])
;;; NOTE it has no concept of "groups" yet

;;; Some further notes for the future:
;;; - We can use { rank = same } blocks to make layout nicer
;;;   - see: http://graphs.grevian.org/example
;;; - We can apparently use syntax: node -> { node node node } to make linking moe compact
;;;   - Of dubious utility for us
;;; - Weight parameter can be used to hit which edges should be shorter

;;; Data structures constructors.

(defun trc/wg--make-connection (dependency-id relation-type &rest props)
  "Encode a connection to DEPENDENCY-ID node.
RELATION-TYPE is one of the supported relation types.
PROPS are other properties, unspecified as of yet."
  (list* :dependency-id dependency-id :relation-type relation-type props))

(defun trc/wg--make-node (id title todo-state tags first-sentence)
  "Encode a node representation.
ID is an org-id, TITLE is unformatted headline content,
TODO-STATE is the TODO keyword, TAGS are org mode tags,
FIRST-SENTENCE is the first sentence of the entry."
  (list :id id :title title :todo-state todo-state :tags tags :first-sentence first-sentence))


;;; Building up the dependency graph.

(defun trc/wg--node-eligible-p (headline)
  "True if HEADLINE is eligible for graphing."
  (not (null (org-element-property :ID headline))))

(defun trc/wg--node-identifier-from-org-headline (headline)
  "Compute an identifier to use for the node from its HEADLINE.
That is either org-id or its title."
  (or (org-element-property :ID headline) (org-element-property :raw-value headline)))

(defun trc/wg--node-from-org-headline (headline)
  "Turn a parsed org mode HEADLINE into a NODE."
  (trc/wg--make-node (trc/wg--node-identifier-from-org-headline headline)
                     (org-element-property :raw-value headline)
                     (org-element-property :todo-keyword headline)
                     (org-element-property :tags headline)
                     nil                ;TODO first-sentence
  ))

(defun trc/wg--parse-edna-blockers (blockers)
  "Turn BLOCKERS into a list of (ID TYPE).
BLOCKERS are a string the form: id(foo bar baz)."
  (when (and blockers
             (string-match "ids(\\(.*\\))" blockers))
    ;; FIXME may not trim up tray spaces!
    (split-string (match-string 1 blockers))))

(defun trc/wg--connections-from-org-headline (headline)
  "Compute all immediate connections for a HEADLINE.
Return value is a list of entries, each of the form:
 (ID (CONNECTION-DATA)), which indicates a node of
ID is connected to another."
  (let ((connections (list)))
    (let ((node-id (trc/wg--node-identifier-from-org-headline headline))
          (parent-id (trc/wg--node-identifier-from-org-headline (org-element-property :parent headline)))
          (edna-blockers (trc/wg--parse-edna-blockers (org-element-property :BLOCKER headline))))
      ;; Parent-child dependency
      (when (trc/wg--node-eligible-p (org-element-property :parent headline))
        (push (list parent-id (trc/wg--make-connection node-id :finish-to-finish))
              connections))
      ;; EDNA blockers
      (dolist (blocker edna-blockers)
        (push (list node-id (trc/wg--make-connection blocker :finish-to-start)) connections)))
    (reverse connections)))


;;; Visualizing with Graphviz
(defun trc/wg--compute-node-label (node)
  "Return the label to use for the NODE."
  (let ((title (plist-get node :title))
        (printable-tags (remove "milestone" (plist-get node :tags))))
    (if printable-tags
        (format "<%s<br/><font point-size=\"9\">%s</font>>" title (s-join ":" printable-tags))
     (format "<%s>" title))))

(defun trc/wg--compute-node-attributes (node)
  "Return a string with extra attributes to style the NODE."
  (let ((color "black")
        (fontcolor "black")
        (shape "box")
        (styles (list)))
    ;; Special-case various node types!
    (let ((todo-kw (plist-get node :todo-state))
          (tags (plist-get node :tags)))
      ;; Milestones are star-like
      (when (member "milestone" tags)
        (setf shape "septagon"))

      ;; State determines color
      (setf color (cond ((equalp todo-kw "TODO")
                         "red")
                        ((equalp todo-kw "DOING")
                         "orange")
                        ((equalp todo-kw "DONE")
                         "darkolivegreen3")
                        (t "black")))

      ;; Tasks are rounded
      (unless (null todo-kw)
        (push "rounded" styles))

      ;; Done tasks are dashed, and text is lightened, to diminish them
      (when (equalp todo-kw "DONE")
        (push "dashed" styles)
        (setf fontcolor "darkslategrey")))

    (format "color=\"%s\",fontcolor=\"%s\"shape=\"%s\",style=\"%s\"" color fontcolor shape (s-join "," styles))))

(defun trc/wg--compute-edge-label (connection)
  "Compute label to be put on edge of a CONNECTION, if any."
  ;; TODO any relevant edge labels go here.
  ""
)

(defun trc/wg--compute-edge-attributes (connection)
  "Compute additional styling for CONNECTION edge."
  (if (eq (plist-get connection :relation-type) :finish-to-finish)
      ;; penwidth=\"0.5\",
      "arrowhead=\"onormal\""
    ""))

(defun trc/wg--graphviz-encode-node (node)
  "Write out NODE definition for graphviz."
  (insert (format "\"%s\" [label=%s,%s]\n"
                  (plist-get node :id)
                  (trc/wg--compute-node-label node)
                  (trc/wg--compute-node-attributes node))))

(defun trc/wg--graphviz-encode-connection (from connection)
  "Write out graphviz edge.
FROM is the id of the source node, CONNECTION specifies
the target and properties of the edge."
  ;; TODO: perhaps a label if Finish-to-Finish? Or a different edge style?
  (insert (format "\"%s\" -> \"%s\" [label=\"%s\",%s]\n"
                  from
                  (plist-get connection :dependency-id)
                  (trc/wg--compute-edge-label connection)
                  (trc/wg--compute-edge-attributes connection))))


;;; Tying it all together

(defun trc/compute-org-task-graph ()
  "Return a graph for the org document, which is a (cons connectome nodelist)."
  (let ((connectome (make-hash-table :test 'equal))
        (nodelist (list)))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (item)
        (when (trc/wg--node-eligible-p item)
          (dolist (connection (trc/wg--connections-from-org-headline item))
            (destructuring-bind (source-id link) connection
              (puthash source-id (cons link (gethash source-id connectome (list))) connectome)))
          (push (trc/wg--node-from-org-headline item) nodelist))
        (values)))
    (list connectome (reverse nodelist))))

(defun trc/org-task-graph-to-graphviz (connectome node-list)
  "Generate a dot graph from CONNECTOME and NODE-LIST."
  (with-temp-buffer
    ;; Preamble
    (insert "digraph G {\n")
    (insert "ranksep=0.5\n")
    (insert "nodesep=0.5\n")
    (insert "overlap=\"false\"\n")
    (insert "node [color=\"black\", fontsize=10, margin=\"0.055\" style=\"rounded\"]\n")
    (insert "edge [fontsize=10]\n")

    ;; List of tasks
    (mapc #'trc/wg--graphviz-encode-node node-list)

    ;; Connections
    (maphash (lambda (k v)
               (dolist (conn v)
                 (trc/wg--graphviz-encode-connection k conn)))
             connectome)

    ;; Postamble
    (insert "}\n")
    (buffer-string)))

(provide 'trc-workgraph)
;;; trc-workgraph.el ends here


                        '';
                };
                org-linker-edna = {
                  after = [ "org" "helm" ];
                  enable = true;
                };
                org-linker = {
                  after = [ "org" ];
                  enable = true;
                };
                org-id = {
                  after = [ "org" ];
                  enable = true;
                  config = ''
                          (setq org-id-method 'ts
                                org-id-prefix nil
                                org-id-ts-format "%Y%m%dT%H%M%S%6N"
                          )
                          ;;Create id's agressively
                          (setq org-id-link-to-org-use-id 'create-if-interactive)
                          (defmacro my-add-id-to-heading (heading-func)
                                    `(advice-add ,heading-func :after
                                      #'(lambda (&rest rest-var)
                                          "Add an ID to new headers automatically"
                               (save-excursion (org-id-get-create)))))
                          ; (my-add-id-to-heading 'org-insert-heading)
                          ; (my-add-id-to-heading 'org-meta-return)
                          ; (my-add-id-to-heading 'org-insert-heading-respect-content)
                          ; (my-add-id-to-heading 'org-insert-todo-heading)
                          ; (my-add-id-to-heading 'org-insert-todo-heading-respect-content)

                          (defun my-org-add-ids-to-headings-in-file ()
                            "Add ID properties to all headlines in the current
                            file that do not already have one"
                            (interactive)
                              (org-map-entries 'org-id-get-create))



                        '';
                };
                s = {
                  enable = true;
                };
                org-roam = {
                  after = [ "org" "s" ];
                  enable = true;
                  config = ''
                      (defun my-trim-slug (slug max-chars)
                             (require 's)
                             (let ((trimmed (substring slug 0 (min max-chars (length slug)))))
                             (s-replace-regexp "_+$" "" trimmed)))

                        (setq org-roam-directory (file-truename my-memx-dir)
                          org-roam-node-display-template
                            (concat "''${title:60} "
                              (propertize "''${tags:20}" 'face 'org-tag)
                              (propertize "''${todo:15}" 'face 'org-todo))
                          org-roam-database-connector 'sqlite-builtin
                          org-roam-db-gc-threshold most-positive-fixnum
                          my-org-roam-max-file-slug-chars 80
                          org-roam-capture-templates '(

                            ("i" "Capture into note")
                            ("iq" "capture into note: quote" plain
                              "\n#+begin_quote :source-link %a :date %U\n%i\n#+end_quote\n%?"
                              :target (file "''${id}-%(my-trim-slug \"''${slug}\" my-org-roam-max-file-slug-chars)___%(concat my-memx-version).org")
                              :unnarrowed)
                            ("it" "capture into note: transclude" plain
                              "\n#+transclude: %a %?"
                            :target (file "''${id}-%(my-trim-slug \"''${slug}\" my-org-roam-max-file-slug-chars)___%(concat my-memx-version).org")
                            :unnarrowed)
                          ("p" "New: find no edit (not for insert)" plain
                            "%?"
                            :target (file+head "''${id}-%(substring \"''${slug}\" my-org-roam-max-file-slug-chars)___%(concat my-memx-version).org"
"* SPARK ''${title}  :%(concat my-memx-version):
:PROPERTIES:
:ID: ''${id}-''${slug}
:ROAM_ALIASES:
:CREATED: %U
:END:
")
                          :immediate-finish)
                          ("j" "New: find edit or insert no edit" plain
                            "%?"
                            :target (file+head "''${id}-%(substring \"''${slug}\" 0 (min my-org-roam-max-file-slug-chars (length \"''${slug}\")))___%(concat my-memx-version).org"
"* SPARK ''${title}  :%(concat my-memx-version):
:PROPERTIES:
:ID: ''${id}-''${slug}
:ROAM_ALIASES:
:CREATED: %U
:END:
")
                          :immediate-finish
                          :jump-to-captured)
;
;
;                            ("T" "CITAR: new CITE note" plain
;                              "%?"
;                              :target (file+head "''${id}-''${citekey}___CITE__%(concat my-memx-version).org"
;"* ''${citekey}  :CITE:%(concat my-memx-version):
;:PROPERTIES:
;:ROAM_ALIASES:
;:URL: ''${url}
;:DOI: ''${doi}
;:AUTHOR: ''${author}
;:EDITOR: ''${editor}
;:YEAR: ''${bdate}
;:NOTER_DOCUMENT: %(orb-process-file-field \"''${citekey}\")
;:TITLE: ''${title}
;:BIBTEX_TYPE: ''${btype}
;:KEYWORDS: ''${keywords}
;:ID: ''${id}-''${citekey}
;:CREATED: %U
;:END:
;
;** Summary
;
;*** Authors Goals
;
;*** My interpretation of results
;
;*** ''${citekey} relevance
;:PROPERTIES:
;:ID: ''${id}-''${citekey}-relevance
;:END:
;
;** Notes
;
;*** Note Magnet
;:PROPERTIES:
;:NOTER_PAGE: (1 0 . 0)
;:END:
;
;"))
;                            ("d" "OOB: new CITE note" plain
;                              "%?"
;                              :target (file+head "''${id}-''${citekey}___CITE__%(concat my-memx-version).org"
;"* ''${citekey}  :CITE:%(concat my-memx-version):
;:PROPERTIES:
;:ROAM_ALIASES:
;:URL: ''${url}
;:DOI: ''${doi}
;:AUTHOR: ''${author}
;:EDITOR: ''${editor}
;:YEAR: ''${date} ''${year} ''${issued}
;:NOTER_DOCUMENT: %(orb-process-file-field \"''${citekey}\")
;:TITLE: ''${title}
;:BIBTEX_TYPE: ''${=type=}
;:ID: ''${id}-''${citekey}
;:KEYWORDS: ''${keywords}
;:CREATED: %U
;:END:
;
;
;** Summary
;
;*** Authors Goals
;
;*** My interpretation of results
;
;*** ''${citekey} relevance
;:PROPERTIES:
;:ID: ''${id}-''${citekey}-relevance
;:END:
;
;** Notes
;
;*** Note Magnet
;:PROPERTIES:
;:NOTER_PAGE: (1 0 . 0)
;:END:
;
;"))
;                            ("s" "new: SPARK note" plain
;                            "%?"
;                            :target (file+head "''${id}-''${slug}___SPARK__%(concat my-memx-version).org"
;"* ''${title}  :SPARK:%(concat my-memx-version):
;:PROPERTIES:
;:ID: ''${id}-''${slug}
;:ROAM_ALIASES:
;:CREATED: %U
;:END:
;")
;                            :unnarrowed)
;                            ("c" "new: CONCEPT note" plain
;                            "%?"
;                            :target (file+head "''${id}-''${slug}___CONCEPT__%(concat my-memx-version).org"
;"* ''${title}  :CONCEPT:%(concat my-memx-version):
;:PROPERTIES:
;:ID: ''${id}-''${slug}
;:ROAM_ALIASES:
;:CREATED: %U
;:END:
;")
;                            :unnarrowed)
;                            ("y" "new: QUERY note" plain
;                            "%?"
;                            :target (file+head "''${id}-''${slug}___QUERY__%(concat my-memx-version).org"
;"* ''${title}  :QUERY:%(concat my-memx-version):
;:PROPERTIES:
;:ID: ''${id}-''${slug}
;:ROAM_ALIASES:
;:CREATED: %U
;:END:
;** Overview
;** Conclusion
;Close when conclusion is reached.
;** Processing template
;- [timestamp] [[websearch: keywords]]
;  - [timestamp] [result url] :: relevance
;  - [timestamp] [result url] :: relevance
;    - [[websearch: new idea inspired by result]]
;** Processing
;")
;                            :unnarrowed)
;                            ("e" "new: ENTITY note" plain
;                            "%?"
;                            :target (file+head "''${id}-''${slug}___ENTITY__%(concat my-memx-version).org"
;"* ''${title}  :ENTITY:%(concat my-memx-version):
;:PROPERTIES:
;:ID: ''${id}-''${slug}
;:ROAM_ALIASES:
;:CREATED: %U
;:END:
;
;
;** Overview
;")
;                            :unnarrowed)
;                            ("a" "Activity note types")
;
;                            ("ap" "new: PROJ note" plain
;                            "%?"
;                            :target (file+head "''${id}-''${slug}___PROJ__%(concat my-memx-version).org"
;"* ''${title}  :PROJ:%(concat my-memx-version):
;:PROPERTIES:
;:ID: ''${id}-''${slug}
;:ROAM_ALIASES:
;:CREATED: %U
;:END:
;
;:PURSUES:
;:END:
;
;:SUPPORTED_BY:
;:END:
;
;:FILES_DIRS:
;:END:
;
;** Overview
;")
;                            :unnarrowed)
;                            ("r" "new: RECIPE note" plain
;                            "%?"
;                            :target (file+head "''${id}-''${slug}___RECIPE__%(concat my-memx-version).org"
;"* ''${title}  :RECIPE:%(concat my-memx-version):
;:PROPERTIES:
;:ID: ''${id}-''${slug}
;:ROAM_ALIASES:
;:CREATED: %U
;:END:
;** Expected Results
;** Inputs
;** Procedure
;")
;                            :unnarrowed)
;

                          )

                        )

                        (org-roam-db-autosync-mode)



                      '';
                };
                org-roam-export = {
                  enable = true;
                  after = [ "org-roam" ];
                };
                org-roam-bibtex = {
                  enable = false;
                  after = [ "org-roam" ];
                  config = ''
                        (setq orb-preformat-templates t
                           orb-preformat-keywords '(
                             "=key="
                             "=type="
                             "title"
                             "citekey"
                             "keywords"
                             "url"
                             "doi"
                             "author"
                             "editor"
                             "date"
                             "year"
                             "issued"
                           )
                           orb-process-file-keyword t
                           orb-file-field-extensions '("pdf")
                           orb-roam-ref-format 'org-cite
                        )
                        (org-roam-bibtex-mode)
                      '';
                };
                denote = {
                  enable = false;
                  after = [ "org" ];
                  config = ''
                          (setq denote-directory my-memx-dir
                                denote-infer-keywords t
                                denote-sort-keywords t
                                denote-known-keywords '("agenda" "xagenda" "xnote")
                                denote-prompts '(title keywords date)
                                denote-date-prompt-use-org-read-date t
                                denote-excluded-keywords-regexp '("xnote")
                                denote-allow-multi-word-keywords nil
                                denote-date-format nil
                                denote-backlinks-show-context t)

                          ;; org-todo-statistics-hook could be used to auto-add
                          ;; a denote file to the agenda buffer
                          (defun my-add-to-agenda (state)
                            "Add the agenda tag to any denote notes that have
                            todo entries added to them"
                            (when (plist-get state :to)
                              (denote-keywords-add "agenda")))

                          (add-hook 'org-trigger-hook #'my-add-to-agenda)

                        '';
                };
                consult-notes = {
                  enable = true;
                  after = [ "org-roam" ];
                  config = ''
                        ;(setq consult-notes-file-dir-sources
                        ;  `(("Memx" ?m ,my-memx-dir))
                        ;)
                          ;(consult-notes-org-headings-mode)
                          (consult-notes-org-roam-mode)


;; Search org-roam notes for citations (depends on citar)
(defun consult-notes-org-roam-cited (reference)
  "Return a list of notes that cite the REFERENCE."
  (interactive (list (citar-select-ref
                      :rebuild-cache current-prefix-arg
                      :filter (citar-has-note))))
  (let* ((ids
          (org-roam-db-query [:select * :from citations
                              :where (= cite-key $s1)]
                             (car reference)))
         (anodes
          (mapcar (lambda (id)
                    (org-roam-node-from-id (car id)))
                  ids))
         (template
          (org-roam-node--process-display-format org-roam-node-display-template))
         (bnodes
          (mapcar (lambda (node)
                    (org-roam-node-read--to-candidate node template)) anodes))
         (node (completing-read
                "Node: "
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        ;; get title using annotation function
                        (annotation-function
                         . ,(lambda (title)
                              (funcall org-roam-node-annotation-function
                                       (get-text-property 0 'node title))))
                        (category . org-roam-node))
                    (complete-with-action action bnodes string pred)))))
         (fnode
          (cdr (assoc node bnodes))))
    (if ids
        ;; Open node in other window
        (org-roam-node-open fnode)
      (message "No notes cite this reference."))))

                        '';
                };
                oc = {
                  enable = true;
                  after = [ "org" "citar" "citar-org" ];
                  config = ''
                            (setq org-cite-global-bibliography my-bib-files
                                  org-cite-insert-processor 'citar
                                  org-cite-follow-processor 'citar
                                  org-cite-activate-processor 'citar)
                            (setq org-cite-export-processors `((t csl ,(file-name-concat my-bib-dir "apa.csl"))))
                      '';
                };
                ebib = {
                  enable = false;
                  config = ''
                        (setq ebib-preload-bib-files my-bib-files
                        ebib-bibtex-dialect 'biblatex
                        ebib-file-search-dirs my-refs-dirs
                        ebib-file-associations '()
                        ebib-link-file-path-type 'relative)

                      '';
                };
                citar = {
                  enable = true;
                  after = [ "org" ];
                  config = ''
                        (setq citar-bibliography my-bib-files
                           citar-at-point-function 'embark-act
                           citar-library-paths (list my-ereading-dir)
                        citar-file-additional-files-separator "---")

                        ;; Hacking citar to use org-bibtex
                        ;; see https://gist.github.com/andras-simonyi/eda0daa0b677838022fd7c438b6eadfa

;; Make Citar aware of org-bib(la)tex bibs.
;; TODO: handle props correctly for Org files
(define-advice citar-cache--update-bibliography (:override (bib &optional props))
  (let* ((filename (citar-cache--bibliography-filename bib))
	 (props (or props (citar-cache--get-bibliography-props filename)))
	 (entries (citar-cache--bibliography-entries bib))
	 (messagestr (format "Updating bibliography %s" (abbreviate-file-name filename)))
	 (starttime (current-time)))
    (message "%s..." messagestr)
    (redisplay)	 ; Make sure message is displayed before Emacs gets busy parsing
    (clrhash entries)
    (if (string= (file-name-extension filename) "org")
	(org-map-entries
	 (lambda ()
	   (when-let ((key-w-entry (citeproc-bt-from-org-headline)))
	     (condition-case err
		 (puthash (car key-w-entry) (cdr key-w-entry)
			  entries)
	       (error
		(user-error
		 "Couldn't parse the bib(la)tex entry with key '%s', the error was: %s"
		 (car key-w-entry) err)))))
	 t (list filename))
      (parsebib-parse filename :entries entries :fields (plist-get props :fields)))
    (setf (citar-cache--bibliography-props bib) props)
    (citar-cache--preformat-bibliography bib)
    (message "%s...done (%.3f seconds)" messagestr (float-time (time-since starttime)))))

;; Open corresponding headlines in org-bib(la)tex files too.
(define-advice citar--open-entry (:override (key bib-files))
  (catch 'break
    (dolist (bib-file bib-files)
      (let ((buf (or (get-file-buffer bib-file)
		     (find-buffer-visiting bib-file))))
	(find-file bib-file)
	(widen)
	(goto-char (point-min))
	(let ((orgp (string= "org" (file-name-extension bib-file))))
	  (when (re-search-forward
		 (if orgp (concat ":CUSTOM_ID: " (regexp-quote key))
		   (concat "^@\\(" parsebib--bibtex-identifier
			   "\\)[[:space:]]*[\\(\\{][[:space:]]*"
			   (regexp-quote key) "[[:space:]]*,")) nil t)
	    (when orgp
	      (re-search-backward "^\\*")
	      (org-fold-show-context))
	    (throw 'break t)))
	(unless buf
	  (kill-buffer))))))


                        (defun my-extend-org-bibtex-write ()
                          "Add missing org properties to org-bibtex-write. Use on an org entry created by org-bibtex-write, possibly automate with advice"
                          (interactive)
                          ;;Need
                          ;; ID (includes date anyway, no need for created property)
                          ;; ROAM_REFS
                          ;; ROAM_ALIASES
                          ;; NOTER_DOCUMENT
                          (let ((base-id (org-id-get nil t))
                               (cite-key (org-entry-get nil "CUSTOM_ID")))
                               (org-entry-put nil "ID" (concat base-id "-" cite-key))
                               (org-entry-put nil "ROAM_REFS" (concat "@" cite-key))
                               (org-entry-put nil "ROAM_ALIASES" cite-key)
                               (org-entry-put nil "NOTER_DOCUMENT" (car (bibtex-completion-find-pdf (org-entry-get nil "CUSTOM_ID") nil)))
                               (org-todo "CITE")
                          )
                        )

                        (defun my-add-noter-org-bibtex-write ()
                          "If the PDF was not added when the org-bibtex entry was created, `:NOTER:' will be empty. Once the PDF is added, this function fills in the `:NOTER:' property"
                          (interactive)
                            (org-entry-put nil "NOTER_DOCUMENT" (car (bibtex-completion-find-pdf (org-entry-get nil "CUSTOM_ID") nil))))

                       (defun my-extend-id-with-slug ()
                          ""
                          (interactive)
                          (save-buffer)
                          (let ((base-id (org-id-get nil t)))
                               (org-entry-put nil "ID" (concat base-id "-" (org-roam-node-slug (org-roam-node-at-point)))))
                       )

                        (advice-add #'org-bibtex-write :after #'my-extend-org-bibtex-write)




'';

                };
                citar-org = {
                  enable = true;
                  after = [ "org"];
                };
                citar-capf = {
                  enable = true;
                  command = [ "citar-capf-setup" ];
                  hook = [
                    "(org-mode . citar-capf-setup)"
                  ];
                };
                citar-embark = {
                  enable = true;
                  after = [ "citar" ];
                  config = ''
                        (citar-embark-mode)
                      '';
                };
                citar-org-roam = {
                  enable = true;
                  after = [ "citar" "org-roam" "org-roam-bibtex" ];
                  init = ''

                      '';
                  config = ''
                        (citar-register-notes-source
                          'orb-citar-source (list :name "Org Roam Notes"
                            :category 'org-roam-node
                            :items #'citar-org-roam--get-candidates
                            :hasitems #'citar-org-roam-has-notes
                            :open #'citar-org-roam-open-note
                            :create #'orb-citar-edit-note
                            :annotate #'citar-org-roam--annotate))

                        (setq citar-notes-source 'orb-citar-source
                          citar-org-roam-capture-template-key "T"
                          citar-org-roam-subdir my-memx-dir
                          citar-org-roam-template-fields '(
                            (:citekey "=key=")
                             (:title "title")
                             (:author "author")
                             (:editor "editor")
                             (:keywords "keywords")
                             (:url "url")
                             (:doi "doi")
                             (:bdate "date" "year" "issued")
                             (:btype "=type="))
                        )




                      '';
                  extraConfig = ''
                      '';
                };
                citar-denote = {
                  enable = false;
                  after = [ "citar" "denote" ];
                  config = ''
                          ;; Use citekey as note title
                          (setq citar-denote-title-format nil)
                        '';
                };
                citeproc-bibtex = {
                  enable = true;
                };
                biblio = {
                  enable = true;
                  config = ''
                        (setq biblio-bibtex-use-autokey t
                        biblio-download-directory (concat my-ereading-dir "/refile"))
                      '';
                };
                biblio-bibsonomy = {
                  enable = false;
                  # requires an account
                };
                bibtex = {
                  enable = true;
                  config = ''
                        (setq bibtex-autokey-names 1
                          bibtex-autokey-names-stretch 1
                          bibtex-autokey-name-separator "-"
                          bibtex-autokey-additional-names ".etal"
                          bibtex-autokey-name-case-convert-function 'capitalize
                          bibtex-autokey-year-length 4
                          bibtex-autokey-titlewords 3
                          bibtex-autokey-titlewords-stretch 2
                          bibtex-autokey-titleword-length t
                          bibtex-autokey-titleword-separator "-"
                          bibtex-autokey-year-title-separator ""

                          bibtex-include-OPTcrossref '("InProceedings" "InCollection" "InBook")



                          bibtex-maintain-sorted-entries 'crossref
                          bibtex-entry-format '(opts-or-alts
                            required-fields
                            numerical-fields
                            whitespace
                            ;realign
                            last-comma
                            delimiters
                          ;  sort-fields
                          )
                          bibtex-file-path my-bib-dir
                        )



                        (bibtex-set-dialect 'biblatex)
                      '';
                };
                ol-bibtex = {
                  enable = true;
                  after = [ "org" ];
                  config = ''
                        (setq org-bibtex-prefix "BIB_"
                              org-bibtex-export-arbitrary-fields t
                              org-bibtex-headline-format-function (lambda (entry)
                                (cdr (assq :key entry))))


(defun my-insert-org-bibtex-from-doi (doi)
	"take a doi, get the bibtex, clean up the bibtex to my liking, then return the formatted org-bibtex entry."
	(interactive "*sDOI: " org-mode)
	(let ((biblio-synchronous t)
				(clean-doi
				 (string-remove-prefix "doi.org/"
					(string-remove-prefix "http://"
					 (string-remove-prefix "https://" doi)))))

		(save-excursion
			(with-temp-buffer
				(doi-insert-bibtex clean-doi)
				(goto-char (point-min))
				(org-ref-title-case)
				(bibtex-clean-entry '(4))
				(org-bibtex-read)))

		(org-bibtex-write)
	))
                      '';

                };
                ## Part of helm-bibtex, and used by org-ref
                helm-bibtex = {
                  enable = true;
                  after = [ "citar" ];
                };
                bibtex-completion = {
                  after = [ "citar" ];
                  enable = true;
                  config = ''
                        ;; matches org-cite-global-bibliography
                        (setq bibtex-completion-bibliography my-bib-files
                          bibtex-completion-library-path my-ereading-dir
                          bibtex-completion-find-additional-pdfs t
                        )

                      '';
                };
                org-ref = {
                  after = [ "org" "bibtex" "bibtex-completion" ];
                  enable = true;
                  config = ''
                          (require 'org-ref-refproc)
                          (require 'org-ref-wos)
                          (require 'org-ref-arxiv)
                          (require 'org-ref-scopus)
                          (setq org-ref-bibtex-pdf-download-dir (concat my-ereading-dir "/refile"))
                          ;(add-hook 'org-export-before-parsing-functions #'org-ref-refproc)
'';
                };
                org-ref-helm = {
                  after = [ "org-ref" ];
                  enable = true;
                };
                smart-tabs-mode = {
                  enable = true;
                  config = ''
                          ;(setq-default indent-tabs-mode nil)
                          (setq-default tab-width 2)
                          ;(add-hook 'ess-r-mode-hook
                          ;(lambda () (setq indent-tabs-mode -1)))
                        '';
                };
                ws-butler = {
                  enable = true;
                  config = ''
                          (ws-butler-global-mode)
                        '';
                };
                whitespace = {
                  enable = true;
                  after = [ "zenburn-theme" ];
                  config = ''
                          (setq whitespace-style '(face tabs trailing lines-tail missing-newline-at-eof empty big-indent space-before-tab space-after-tab)
                                whitespace-global-modes '(not magit-mode eat-mode))

                          (add-hook 'whitespace-mode-hook #'(lambda ()
                             (face-remap-add-relative 'whitespace-big-indent
														  :foreground  (cdr (assoc "zenburn-red+2" zenburn-default-colors-alist))
														  :background (cdr (assoc "zenburn-red-2" zenburn-default-colors-alist)))))


                          (global-whitespace-mode)
                          '';
                };
                jinx = {
                  enable = true;
                  bind = {
                    "M-$" = "jinx-correct";
                  };
                  hook = [ "(emacs-startup . global-jinx-mode)"];
                };
                org-remark = {
                  after = [ "org" ];
                  enable = true;
                  config = ''
                          (defun my-org-remark-notes-file-names ()
                                 (concat my-memx-dir
                                   (file-name-base (org-remark-notes-file-name-function))
                                   "___org-remark.org"))
                          (setq org-remark-notes-display-buffer-action '((display-buffer-in-side-window) (side . right) (slot . 1))
                                org-remark-notes-buffer-name "*remark file notes*"
                                org-remark-use-org-id t
                                org-remark-source-file-name #'abbreviate-file-name
                                org-remark-notes-file-name #'my-org-remark-notes-file-names)
                          (org-remark-global-tracking-mode +1)
                        '';
                };
                org-noter = {
                  after = [ "org" "pdf-tools" "nov" "djvu"];
                  enable = true;
                  command = [ "org-noter" ];
                  config = ''
                        ;; hack to fix problems with compilation and macros not working
                        ;; The following function is copied straight out of org-noter-pdf.el
(defun org-noter-pdf--show-arrow ()
  ;; From `pdf-util-tooltip-arrow'.
  (pdf-util-assert-pdf-window)
  (let* (x-gtk-use-system-tooltips
         (arrow-top  (aref org-noter--arrow-location 2)) ; % of page
         (arrow-left (aref org-noter--arrow-location 3))
         (image-top  (if (floatp arrow-top)
                         (round (* arrow-top  (cdr (pdf-view-image-size)))))) ; pixel location on page (magnification-dependent)
         (image-left (if (floatp arrow-left)
                         (floor (* arrow-left (car (pdf-view-image-size))))))
         (dx (or image-left
                 (+ (or (car (window-margins)) 0)
                    (car (window-fringes)))))
         (dy (or image-top 0))
         (pos (list dx dy dx (+ dy (* 2 (frame-char-height)))))
         (vscroll (pdf-util-required-vscroll pos))
         (tooltip-frame-parameters
          `((border-width . 0)
            (internal-border-width . 0)
            ,@tooltip-frame-parameters))
         (tooltip-hide-delay 3))

    (when vscroll
      (image-set-window-vscroll vscroll))
    (setq dy (max 0 (- dy
                       (cdr (pdf-view-image-offset))
                       (window-vscroll nil t)
                       (frame-char-height))))
    (when (overlay-get (pdf-view-current-overlay) 'before-string)
      (let* ((e (window-inside-pixel-edges))
             (xw (pdf-util-with-edges (e) e-width))
             (display-left-margin (/ (- xw (car (pdf-view-image-size t))) 2)))
        (cl-incf dx display-left-margin)))
    (setq dx (max 0 (+ dx org-noter-arrow-horizontal-offset)))
    (pdf-util-tooltip-in-window
     (propertize
      " " 'display (propertize
                    "\u2192" ;; right arrow
                    'display '(height 2)
                    'face `(:foreground
                            ,org-noter-arrow-foreground-color
                            :background
                            ,(if (bound-and-true-p pdf-view-midnight-minor-mode)
                                 (cdr pdf-view-midnight-colors)
                               org-noter-arrow-background-color))))
     dx dy)))

                      '';
                };
                pdf-tools = {
                  enable = true;
                  init = ''
                      '';
                };
                pdf-loader = { #part of pdf-tools
                  enable = true;
                  config = ''
                        (pdf-loader-install)

;; fix, because temporary squashfs files contain a colon
;; which can't be used in ntfs, so tmp dir must
;; not be in para, but only when opening pdfs from squashfs
(defun redefine-tmp (modfunc &rest args)
	"redefine tmpdir to '/tmp/pdftools'"
	(let ((pdf-util--base-directory "/tmp")
				(temporary-file-directory "/tmp"))
							(apply modfunc args)))
(advice-add 'pdf-util-dedicated-directory :around #'redefine-tmp)

                      '';
                };

                shrface = {
                  enable = true;
                  config = ''
                          (shrface-basic)
                          (shrface-trial)
                          (shrface-default-keybindings)
                          (setq shrface-href-versatile t)
                        '';
                };
                shr-tag-pre-highlight = {
                  after = [ "shr" "shrface" ];
                  enable = true;
                  config = ''
                          (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight))
                          (defun shrface-shr-tag-pre-highlight (pre)
                            "Highlighting code in PRE."
                            (let* ((shr-folding-mode 'none)
                                  (shr-current-font 'default)
                                  (code (with-temp-buffer
                                    (shr-generic pre)
                          ;; (indent-rigidly (point-min) (point-max) 2)
                                             (buffer-string)))
                                  (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                              (let ((sym (language-detection-string code)))
                                (and sym (symbol-name sym)))))
                                  (mode (and lang
                                    (shr-tag-pre-highlight--get-lang-mode lang)))
                                    (light (eq (frame-parameter nil 'background-mode) 'light)))
                                    (shr-ensure-newline)
                                    (shr-ensure-newline)
                                    (setq start (point))
                                    (insert
                                      (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
                                      (or (and (fboundp mode)
                                      (with-demoted-errors "Error while fontifying: %S"
                                      (shr-tag-pre-highlight-fontify code mode)))
                                        code)
                                      (propertize "\n#+END_SRC" 'face 'org-block-end-line ))
                                    (shr-ensure-newline)
                                    (setq end (point))
                                    (if light
                                      (add-face-text-property start end '(:background "#D8DEE9" :extend t))
                                      (add-face-text-property start end `(:background  ,(zenburn-with-color-variables zenburn-bg-08) :extend t)))
                                      (shr-ensure-newline)
                                      (insert "\n")))

                          (when (version< emacs-version "26")
                          (with-eval-after-load 'eww
                             (advice-add 'eww-display-html :around
                               'eww-display-html--override-shr-external-rendering-functions)))
                        '';
                };
                org-web-tools = {
                  after = [ "org" "shrface"  ];
                  enable = true;
                  config = ''
                          (advice-add 'org-web-tools--html-to-org-with-pandoc :override 'shrface-html-convert-as-org-string)

                          (defun request-url-readable (url)
                            (interactive "sRequest url: ")
                            (require 'shrface)
                            (require 'request)
                            (require 'org-web-tools)
                            (request url
                              :parser 'buffer-string
                              :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
                              :sync nil
                              :success (cl-function
                            (lambda (&key data &allow-other-keys)
                            (let* ((web (org-web-tools--eww-readable data))
                                  (title (car web))
                                  (html (cdr web))
                              (shrface-org-title title)
                              (shrface-request-url url))
                              (shrface-html-export-as-org html))))))
                        '';
                };


                nov = {
                  after = [ "shrface" ];
                  enable = true;
                  init = ''
                          (add-hook 'nov-mode-hook #'shrface-mode)
                        '';
                  config = ''
                          (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
                          (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
                        '';
                };

                djvu = {
                  enable = true;
                };

                eww = {
                  after = [ "shrface" ];
                  enable = true;
                  init = ''
                          (add-hook 'eww-after-render-hook #'shrface-mode)
                        '';
                };
                ##Packages loaded when needed
                free-keys = {
                  enable = true;
                  command = [ "free-keys" ];
                };
                ess-site = {
                  enable= true;
                  mode = [ ''
                          ("\\.Rd\\'" . Rd-mode)
                          ("DESCRIPTION\\'" . conf-colon-mode)
                          ("\\.Rd\\'" . Rd-mode)
                          ("DESCRIPTION\\'" . conf-colon-mode)
                          ("\\.[Rr]out\\'" . ess-r-transcript-mode)
                          ("CITATION\\'" . ess-r-mode)
                          ("NAMESPACE\\'" . ess-r-mode)
                          ("\\.[rR]profile\\'" . ess-r-mode)
                          ("\\.[rR]\\'" . ess-r-mode)
                          ("/R/.*\\.q\\'" . ess-r-mode)
                          ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
                          ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
                          ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
                          ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
                        '' ];
                  extraConfig = ''
                          :interpreter (("r" . ess-r-mode)
                          ("Rscript" . ess-r-mode))
                        '';
                };
                ess = {
                  after = [ "ess-site" ];
                  enable = true;
                  config = ''
                          ;;(setq inferior-ess-r-program "radian") ;;  ESS can't speak radian's language
                          (setq ess-use-flymake nil)
                        '';
                  hook = [
                    "(ess-r-mode . (lambda ()
                           (ess-set-style 'RStudio)))"
                  ];
                };
                nix-mode = {
                  enable = true;
                  mode = [ ''"\\\\.nix\\\\'"''];
                };
                nix-flake = {
                  enable = true;
                  command = [ "nix-flake" ];
                  after = [ "nix-mode" ];
                };
                helm-nixos-options = {
                  enable = true;
                  command = [ "helm-nixos-options" ];
                  mode = [ ''"\\\\.nix\\\\'"'' ];
                };
                ob-nix = {
                  enable = true;
                };
                ob-d2 = {
                  enable = true;
                  config = ''
(add-to-list 'org-babel-load-languages (cons (intern "d2") t))
                          (org-babel-do-load-languages
                            'org-babel-load-languages
                            org-babel-load-languages)
                      '';
                };
                ob-latex = {
                  enable = true;
                  after = [ "org" ];
                  config = ''
                      (add-to-list 'org-latex-packages-alist '("" "gensymb" t))
                      (setq
                        org-latex-compiler "lualatex"
                        org-babel-latex-preamble (lambda (_)
                        "\\documentclass[tikz,crop]{standalone}
                         \\def\\pgfsysdriver{pgfsys-tex4ht.def}
                         "))

                      (add-to-list 'org-babel-load-languages (cons (intern "latex") t))
                          (org-babel-do-load-languages
                            'org-babel-load-languages
                            org-babel-load-languages)
                        (setq org-latex-pdf-process
                          '("lualatex -shell-escape -interaction nonstopmode -output-directory=%o %f"
"lualatex -shell-escape -interaction nonstopmode -output-directory=%o %f")
                               luamagick '(luamagick :programs ("lualatex" "convert")
:description "pdf -> png"
:message "You need to install lualatex and imagemagick"
:use-xcolor t
:image-input-type "pdf"
:image-output-type "png"
:image-size-adjust (1.0 . 1.0)
:latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
:image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))

                        (add-to-list 'org-preview-latex-process-alist luamagick)
                        ;(setq org-preview-latex-default-process 'luamagick)


                      '';
                };
                tex = {
                  extraPackages = [ pkgs.auctex ];
                  enable = true;
                  after = [ "ob-latex" ];
                };
                d2-mode = {
                  enable = true;
                  config = ''
                        (setq d2-tmp-dir temporary-file-directory)
                      '';
                };
                ob-plantuml = {
                  enable = true;
                  config = ''
                        (setq org-plantuml-exec-mode 'plantuml)
                      '';
                };
                plantuml-mode = {
                  enable = true;
                  after = [ "org" ];
                  mode = [ ''"\\\\.plantuml\\\\'"'' ];
                  config = ''
                        (setq plantuml-default-exec-mode 'executable)
                        (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
                          (add-to-list 'org-babel-load-languages (cons (intern "plantuml") t))
                          (org-babel-do-load-languages
                            'org-babel-load-languages
                            org-babel-load-languages)
                   '';
                };
                flycheck-plantuml = {
                  enable = true;
                  after = [ "flycheck" "plantuml-mode" ];
                  config = ''
                        (flycheck-plantuml-setup)
                      '';
                };

                eglot = {
                  enable = true;
                  command = [ "eglot" "eglot-ensure" ];
                  config = ''
                          ;(add-to-list 'eglot-server-programs
                           ;   `(nix-mode . ("nil" :initializationOptions
                            ;     (:nix
                             ;     (:flake (:autoArchive t
                              ;    :autoEvalInputs t
                               ;   :nixpkgsInputName "nixpkgs-unstable"))))))
                        ;; Eglot already has entries for R and nix
                        '';
                  hook = [
                    "(nix-mode . eglot-ensure)"
                    "(ess-r-mode . eglot-ensure)"
                    "(R-mode . eglot-ensure)"
                  ];
                };
                flycheck-eglot = {
                  enable = true;
                  after = [ "flycheck" "eglot" ];
                  config = ''
                               (global-flycheck-eglot-mode 1)
                      '';
                };
                csv-mode = {
                  enable = true;
                };
              };
            };
          };
        };
      };
      emacs-mwe = {config, pkgs, ...}:
        {
          services = {
            emacs = {
              enable = true;
              defaultEditor = true;
              ## Allow server to start with graphics so wayland session is
              ## correctly detected by server
              # socketActivation.enable = true;
              startWithUserSession = "graphical";
            };
          };
          programs = {
            emacs = {
              enable = true;
              package = pkgs.emacs-pgtk;
              extraPackages = epkgs: [
              ];
              init = {
                enable = true;
                packageQuickstart = true;
                recommendedGcSettings = true;
                startupTimer = true;
                earlyInit = "";
                postlude = ''
                      ; Seems to break if called too early
                      ; (citar-org-roam-mode)

                      ;; Local Variables:
                      ;; End:
                    '';
                usePackage = {
                  djvu = {
                    enable = true;
                  };

                  org-noter = {
                    after = [ "org" "pdf-tools" "nov" "djvu"];
                    enable = true;
                    command = [ "org-noter" ];
                  };
                  pdf-tools = {
                    enable = true;
                    init = ''
                      '';
                  };
                  pdf-loader = { #part of pdf-tools
                    enable = true;
                    config = ''
                        (pdf-loader-install)
                      '';
                  };

                };

              };
            };

          };
        };
      tex-full = {config, pkgs, ...}:
        {
          programs.texlive = {
            enable = true;
            extraPackages = tpkgs: {
              inherit (tpkgs)
                scheme-basic
                scheme-full
                biber
                collection-bibtexextra
                collection-mathscience
                collection-latexrecommended
                collection-latexextra
                collection-pictures
                collection-plaingeneric
                collection-fontsrecommended
                collection-xetex
                collection-luatex
                dvisvgm
                dvipng
                pgf;
            };
          };
        };

    };
    devShells."x86_64-linux" = {
      secureboot-tools = let
        pkgs = import nixpkgs-unstable {system = "x86_64-linux";};
      in
        pkgs.mkShell {
          name = "secureboot_tools_shell";
          version = "1";
          buildInputs = with pkgs; [
            sbsigntool
            sbctl
            efitools
          ];
        };
    };
    packages."x86_64-linux" = {
    };
    overlays = {
      lsix_configured = final: prev: {
        lsix = prev.lsix.overrideAttrs (oldAttrs: {
          postInstall = ''
            substituteInPlace $out/bin/lsix \
              --replace tilesize=120 tilesize=390 \
              --replace \#fontfamily=Dejavu-Sans fontfamily=Dejavu-Sans \
              --replace fontsize=\$\(\(tilewidth/10\)\) fontsize=20
          '';
        });
      };

    };

    nixosConfigurations = {
      prime-ai-bootstrap = nixpkgs-unstable.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.prime-ai.bootstrap_hardware
          self.nixosModules.system_config
          self.nixosModules.prime-ai.bootstrap_user
          inputs.ragenix.nixosModules.age
        ];
      };
      prime-ai = nixpkgs-unstable.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.prime-ai.hardware_config
          self.nixosModules.prime-ai.networking
          self.nixosModules.prime-ai.hardware_config_tuxedo
          self.nixosModules.prime-ai.hardware_shared_crypt
          self.nixosModules.prime-ai.syncthing
          self.nixosModules.prime-ai.tailscale
          self.nixosModules.prime-ai.phil_home
          self.nixosModules.prime-ai.phil_user
          self.nixosModules.prime-ai.gui

          self.nixosModules.system-conf.network_fs
          self.nixosModules.system-conf.wifi_secrets
          self.nixosModules.system-conf.secure_boot

          self.nixosModules.system-conf.openssh
          self.nixosModules.system-conf.allow-unfree
          self.nixosModules.system-conf.locale_au
          self.nixosModules.system-conf.cli
          self.nixosModules.system-conf.gui
          self.nixosModules.system-conf.udisks
          self.nixosModules.system-conf.fonts
          self.nixosModules.system-conf.lock-root
          self.nixosModules.system-conf.nix-config
          self.nixosModules.system-conf.stateversion
          self.nixosModules.system-conf.slurm-server
          self.nixosModules.window-managers.hyprland

          # self.nixosModules.bib_reorganise # riskier when using org-bibtex, may be editing notes when timer kicks in.
          self.nixosModules.gui.inkscape # works best when GTK is set up
          self.nixosModules.cli.python-system
          self.nixosModules.cli.spell_checkers
          self.nixosModules.cli.direnv
          #self.nixosModules.window-managers.xfce_desktop

          self.nixosModules.cli.ryzen_monitor

          # Not sure how this fits in
          inputs.ragenix.nixosModules.age
        ];
      };

      phil-vm = nixpkgs-unstable.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          # vm specific config
          self.nixosModules.x1carbon-vm.boot
          self.nixosModules.x1carbon-vm.networking
          self.nixosModules.x1carbon-vm.trackpad
          # self.nixosModules.x1carbon-vm.desktop
          self.nixosModules.x1carbon-vm.fs
          self.nixosModules.x1carbon-vm.phil_user
          self.nixosModules.x1carbon-vm.vm-settings

          # Not sure how this fits in
          inputs.ragenix.nixosModules.age

          # general system modules
          self.nixosModules.system-conf.openssh
          self.nixosModules.system-conf.allow-unfree
          self.nixosModules.system-conf.locale_au
          self.nixosModules.system-conf.fonts
          self.nixosModules.system-conf.lock-root
          self.nixosModules.system-conf.nix-config
          self.nixosModules.system-conf.stateversion

          # self.nixosModules.window-managers.hyprland
          self.nixosModules.window-managers.xfce_desktop

          self.nixosModules.cli.python-system
          self.nixosModules.cli.spell_checkers
          # self.nixosModules.cli.direnv

          self.nixosModules.system-conf.network_fs

          ## Need to get phil_home in here somehow
          # self.nixosModules.prime-ai.phil_home

        ];
      };

    };
    nixOnDroidConfigurations = {
      galaxym62 = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        modules = [];
      };
    };
    # For managing with home-manager cli.
    # I prefer the phil_home module
    homeConfigurations = {
      "phil@prime-ai-nixos" = inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs-unstable.legacyPackages.x86_64-linux;

        modules = [
          self.nixosModules.phil_home
        ];

      };
    };
  };
}
