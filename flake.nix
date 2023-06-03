{
  description = "Prime-AI Nixos Configuration";

  inputs  = {
    nixpkgs-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
      #url = "github:NixOS/nixpkgs/master"; #temporary change for bug in nixos
      # url = "github:NixOS/nixpkgs?rev=5abc896edad307816c03d9a672cc8fcf683e8f35"; #temporary change for bug in nixos
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    ragenix = {
      url = "github:yaxitech/ragenix";
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
    hyprland = {
      url = "github:hyprwm/Hyprland";
      #not following nixpkgs to get caching
    };
    tuxedo-nixos = {
      url = "github:blitz/tuxedo-nixos";
      # Avoid pulling in the nixpkgs that we pin in the tuxedo-nixos repo.
      # This should give the least surprises and saves on disk space.
      # inputs.nixpkgs.follows = "nixpkgs-unstable"; # not working with nixpkgs unstable yet
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
  };

  outputs = {self, nixpkgs-unstable, ...}@inputs: {
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
    nixosModules = {
      prime-ai_hardware_config = { config, lib, pkgs, modulesPath, ...}:
      let
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
                  "governor": "powersave",
                  "energyPerformancePreference": "balance_performance",
                  "noTurbo": false,
                  "onlineCores": 24,
                  "scalingMinFrequency": 2200000,
                  "scalingMaxFrequency": 4700000
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
                  "useBrightness": false
                },
                "cpu": {
                  "useMaxPerfGov": false,
                  "governor": "powersave",
                  "energyPerformancePreference": "balance_performance",
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
          (modulesPath + "/installer/scan/not-detected.nix")
          inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
          inputs.nixos-hardware.nixosModules.common-cpu-amd
          inputs.nixos-hardware.nixosModules.common-pc-ssd
          inputs.nixos-hardware.nixosModules.common-pc
          inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
          inputs.tuxedo-nixos.nixosModules.default
        ];
        #powerManagement.cpuFreqGovernor = "performance"; #forced to schedutil by tuxedo control center
        hardware.tuxedo-control-center.enable = true;
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
        ## Needed by tuxedo-nixos
        ## Supposed to be set by tuxedo-nixos, but
        ## not being seen for some reason
        nixpkgs.config.permittedInsecurePackages = [
          "openssl-1.1.1u"
          "openssl-1.1.1t"
          "nodejs-14.21.3"
          "electron-13.6.9"
        ];
        boot = {
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
      xfce_desktop = {config, pkgs, ...}:
      {
        services.xserver.windowManager.dwm.enable = true;
        services.xserver = {
          enable = true;
          desktopManager = {
            xterm.enable = false;
            xfce.enable = true;
          };
          displayManager.defaultSession = "xfce";
          dpi = 300;
        };
      };
      prime-ai_hardware_shared_crypt = { config, lib, pkgs, ...}:
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
      secure_boot = {config, pkgs, lib, ...}:
      {
        imports = [
          inputs.lanzaboote.nixosModules.lanzaboote
        ];
        #boot.bootspec.enable = true; #duplicated from prime-ai_hardware_config
        boot.loader.systemd-boot.enable = lib.mkForce false;
        boot.lanzaboote = {
          enable = true;
          pkiBundle = "/etc/secureboot";
        };
      };
      network_fs = {config, pkgs, ...}:
      let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=600,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,uid=1001,gid=100";
      in
      {
        age.secrets.cifs_dpbagje_share.file = "/secrets/agenix/cifs_dpbagje_share.age";
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
        age.secrets.wpa_pwd_env.file = "/secrets/agenix/wpa_pwd.env.age";
        networking.wireless = {
          environmentFile = config.age.secrets.wpa_pwd_env.path;
          networks = {
            PBAGJmob = {
              psk = "@phone_psk@";
              priority = 10;
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
          };
        };
      };
      phil_user = {config, pkgs, ...}:
      {
        age.secrets.user_phil_pwd.file = "/secrets/agenix/user_phil_pwd.age";
        users.users = {
          phil = {
            isNormalUser = true;
            extraGroups = ["wheel"];
            passwordFile = config.age.secrets.user_phil_pwd.path;
            uid = 1001;
          };
        };
      };
      prime_ai_tailscale = {config, pkgs, ...}:
      {
        age.secrets.prime_ai_tailscale.file = "/secrets/agenix/prime_ai_tailscale.age";
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
      #ssh_public_config
      system_config = {config, pkgs, ...}:
      {
        nixpkgs.config.allowUnfree = true;
        system.stateVersion = "21.11";
        boot.loader.systemd-boot = {
          #enable = true; #set to false by lanzaboote
          editor = false;  #don't allow kernel cli editing before boot
        };
        boot.loader.efi.canTouchEfiVariables = true;
        boot.kernelPackages = pkgs.linuxPackages_latest;

        networking = {
          hostName = "prime-ai-nixos";
          firewall = {
            allowedTCPPorts = [ ];
            allowedUDPPorts = [ ];
          };
          wireless = {
            enable = true;
          };
          interfaces = {
            enp4s0.useDHCP = true;
            wlp5s0.useDHCP = true;
          };
        };

        time.timeZone = "Australia/Brisbane";
        i18n.defaultLocale = "en_AU.UTF-8";

        # services.xserver = {
          #   enable = true;
          #   displayManager.lightdm.enable = true;
          #   displayManager.defaultSession = "none+dwm";
          #   windowManager.dwm.enable = true;
          # };

          #sound.enable = true;
          #hardware.pulseaudio.enable = true;

          users.mutableUsers = false;
          users.users = {
            root.hashedPassword = "*";
          };

          environment.systemPackages = with pkgs; [
            vim
            git
            gitSVN
            wget
            firefox
            # nyxt
            st
            #agenix.packages.x86_64-linux.default #nix run github:ryantm/agenix -- --help
            python3
            openssl
            # geekbench_6
            pqiv
            gthumb
            gtk3
            imagemagickBig
            ripgrep
            nil # nix language server
          ];

          fonts = {
            fonts = with pkgs; [
              (nerdfonts.override { fonts = [ "FiraCode" "RobotoMono" ]; })
            ];

            fontconfig.defaultFonts = {
              monospace = [ "RobotoMono" ];
            };
          };


          nix = {
            package = pkgs.nixVersions.unstable;
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
            };
          };

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
      hyprland-prime-ai = {config, pkgs, ...}:
      {
        imports = [
          inputs.hyprland.nixosModules.default
        ];

        nixpkgs.overlays = [
          self.overlays.lsix_configured
        ];

        programs.hyprland = {
          enable = true;
          nvidiaPatches = true;
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
        nix.settings = {
          substituters = ["https://hyprland.cachix.org"];
          trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
        };
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
      phil_home = {config, pkgs, ...}: {
        imports = [
          inputs.home-manager.nixosModules.home-manager
        ];

        nixpkgs.overlays = [
          inputs.emacs-overlay.overlays.default
        ];
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
                BIG=/para/areas/bibliography__bib/readings.bib
                # small, changes more frequently
                SMALL=/para/areas/bibliography__bib/new_refs.bib

                # size of small before script actually does anything
                MAXSIZE=5000

                # get file size
                FILESIZE=$(stat -c%s "$SMALL")

                if ((FILESIZE > MAXSIZE)); then
                    # when $SMALL exceeds $MAXSIZE, move its content to $BIG
                    cat "$SMALL" >> "$BIG"
                    echo "" > "$SMALL"
                fi
              '';
        };


        nix.settings = {
          substituters = ["https://nix-community.cachix.org"];
          trusted-public-keys = ["nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="];
        };
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;

          #config inserted before use-package
          users.phil = {
            imports = let
              nurNoPkgs = import inputs.nur { pkgs = null; nurpkgs = pkgs; };
            in [
              inputs.hyprland.homeManagerModules.default
              nurNoPkgs.repos.rycee.hmModules.emacs-init
            ];
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
                r-config = {
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
            };
            services = {
              gpg-agent = {
                enable = true;
                defaultCacheTtl = 72000;
                pinentryFlavor = "gtk2"; # other interesting flavours emacs tty curses
                extraConfig = ''
                  allow-emacs-pinentry
                  allow-loopback-pinentry
                '';

                maxCacheTtl = 72000;

              };
              emacs = {
                enable = true;
                defaultEditor = true;
                ## Allow server to start with graphics so wayland session is
                ## correctly detected by server
                socketActivation.enable = true;
              };
            };

            programs = {
              emacs = {
                enable = true;
                package = pkgs.emacs-pgtk;
                extraPackages = epkgs: [
                ];
                overrides = final: prev: {
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
                            (shell-command-to-string "wl-paste -n | tr -d \r")))

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
                      ;; Local Variables:
                      ;; no-byte-compile: t
                      ;; End:
                    '';

                    #Packages configured
                    usePackage = {
                      ## Startup packages. 'After' needs to flow back to an always-loaded package
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
                        '';
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
                        enable = true;
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
                          (objed-mode)
                          (setq objed-disabled-modes '(epa-key-list-mode
                              magit-mode))


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
                                auto-save-file-name-transforms (list (list ".*" auto-save-list-file-prefix t)))
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
                            prism-desaturations '(0 0 0 0)
                            prism-lightens '(0 5 10 20)
                            prism-num-faces 24)
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
                          (setq sp-autowrap-region nil
                                sp-autodelete-pair nil
                                sp-autodelete-opening-pair nil
                                sp-autodelete-closing-pair nil
                                sp-autoinsert-pair nil
                                sp-autodelete-wrap nil
                                sp-autoskip-opening-pair nil
                                sp-autoskip-closing-pair nil)
                          (smartparens-global-mode)
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

                        '';
                      };
                      isend-mode = {
                        enable = true;
                      };
                      magit = {
                        enable = true;
                        config = ''
                          (setq magit-refresh-status-buffer nil)
                        '';
                      };
                      forge = {
                        enable = true;
                      };
                      git-timemachine = {
                        enable = true;
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
                      consult = {
                        enable = true;
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

                          ;; Use Consult to select xref locations with preview
                          (setq xref-show-xrefs-function #'consult-xref
                          xref-show-definitions-function #'consult-xref)
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
                        enable = true;
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
                          (org-babel-do-load-languages
                            'org-babel-load-languages
                            '((emacs-lisp . t)
                            (R . t)))

                          ;;Allow code blocks to execute without asking me every time
                          ;; for safetly though, don't allow C-c C-c to evaluate blocks
                           (setq org-confirm-babel-evaluate (lambda (lang src) (if (string= lang "R") nil t))
                                 org-babel-no-eval-on-ctrl-c-ctrl-c t)

                          ;;never run blocks on export. Creates consisent results for R async session blocks.
                          (add-to-list 'org-babel-default-header-args '(:eval . "never-export"))

                          ;;set up todo entries
                          (setq org-todo-keywords '((sequence "PROBLEM" "|" "SOLVED" "SKIP")
                          (sequence "ATTEMPT" "|" "FAIL" "SUCCESS")
                          (sequence "TODO(t)" "NEXT(n!)" "WAIT(b@/!)" "IDEA(i@)" "|" "NEVER(x@)" "DONE(d!)")
                          (sequence "ADD" "FIND" "SKIM(1!)" "1ST_READ(2!)" "1ST_MARG(3!)" "2ND_READ(4!)" "2ND_MARG(5!)" "3RD_READ(6!)" "3RD_MARG(7!)" "GOLD(8!)" "|" "REF(9!)" "DROP(k)")
                          ))

                          (setq org-M-RET-may-split-line nil)

                          (setq org-log-into-drawer t
                                org-log-redeadline t
                                org-log-reschedule t
                                org-log-done t)

                          (setq org-image-actual-width '(800))

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
                      #org link library
                      ol = {
                        enable = true;
                        after = [ "org" ];
                        config = ''
                          (setq org-link-abbrev-alist
                          '(("websearch"      . "https://start.duckduckgo.com/?q=%s")))
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
                        enable = true;
                        bindLocal = {
                          org-mode-map = {
                            "C-c C-i" = "org-slt-phdyellow";
                          };
                        };
                      };
                      org-transclusion = {
                        after = [ "org" ];
                        enable = true;
                      };
                      org-edna = {
                        after = [ "org" ];
                        enable = true;
                        config = ''
                          (org-edna-mode)
                        '';
                      };
                      org-linker-edna = {
                        after = [ "org" ];
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
                                org-id-prefix "org")
                          ;;Create id's agressively
                          (setq org-id-link-to-org-use-id 'create-if-interactive)
                          (defmacro my-add-id-to-heading (heading-func)
                                    `(advice-add ,heading-func :after
                                      #'(lambda (&rest rest-var)
                                          "Add an ID to new headers automatically"
                               (save-excursion (org-id-get-create)))))
                          (my-add-id-to-heading 'org-insert-heading)
                          (my-add-id-to-heading 'org-meta-return)
                          (my-add-id-to-heading 'org-insert-heading-respect-content)
                          (my-add-id-to-heading 'org-insert-todo-heading)
                          (my-add-id-to-heading 'org-insert-todo-heading-respect-content)

                          (defun my-org-add-ids-to-headings-in-file ()
                            "Add ID properties to all headlines in the current
                            file that do not already have one"
                            (interactive)
                              (org-map-entries 'org-id-get-create))


                          (setq org-agenda-files "/para/areas/memx"
                                org-agenda-file-regexp "\\`[^.].*_agenda.*\\.org\\'")
                        '';
                      };
                      denote = {
                        enable = true;
                        after = [ "org" ];
                        config = ''
                          (setq denote-directory "/para/areas/memx/"
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
                        after = [ "denote" ];
                        config = ''
                          (setq consult-notes-denote-display-id nil
                                consult-notes-denote-dir nil)

                          (consult-notes-org-headings-mode)
                          (consult-notes-denote-mode)
                        '';
                      };

                      citar = {
                        enable = true;
                        after = [ "org" "oc" ];
                        config = ''
                            (setq org-cite-global-bibliography
                                    '("/para/areas/bibliography__bib/new_refs.bib"
                                      "/para/areas/bibliography__bib/readings.bib")
                                  org-cite-insert-processor 'citar
                                  org-cite-follow-processor 'citar
                                  org-cite-activate-processor 'citar
                                  citar-at-point-function 'embark-act)
                        '';
                      };
                      citar-denote = {
                        enable = true;
                        after = [ "citar" "denote" ];
                        config = ''
                          ;; Use citekey as note title
                          (setq citar-denote-title-format nil)
                        '';
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
                                    (face-remap-add-relative 'whitespace-big-indent (zenburn-with-color-variables `(:foreground ,zenburn-red+2 :background ,zenburn-red-2)))))
                          (global-whitespace-mode)
                          '';
                      };
                      jinx = {
                        enable = true;
                        hook = [ "(emacs-startup . global-jinx-mode)"];
                      };
                      org-remark = {
                        after = [ "org" ];
                        enable = true;
                        config = ''
                          (defun my-org-remark-notes-file-names ()
                                 (concat "/para/areas/memx/"
                                   (file-name-base (org-remark-notes-file-name-function))
                                   " -- org-remark.org"))
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
                      };
                      pdf-tools = {
                        enable = true;
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
                        '';
                      };
                      nix-mode = {
                        enable = true;
                        mode = [ ''"\\\\.nix\\\\'"''];
                      };
                      nixos-options = {
                        enable = true;
                        mode = [ ''"\\\\.nix\\\\'"'' ];
                      };
                      ob-nix = {
                        enable = true;
                      };
                      eglot = {
                        enable = true;
                        command = [ "eglot" "eglot-ensure" ];
                        config = ''
                        ;; Eglot already has entries for R and nix
                        '';
                        hook = [
                          "(nix-mode . eglot-ensure)"
                          "(ess-r-mode . eglot-ensure)"
                          "(R-mode . eglot-ensure)"
                        ];
                      };
                    };

                };
              };
              bash = {
                enable = true;
              };
              fish = {
                enable = true;
              };
              gpg = {
                enable = true;
              };
              git ={
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

            wayland.windowManager.hyprland = {
              enable = true;
              package = null;
              extraConfig = ''
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
                blur = true
                blur_size = 3
                blur_passes = 1
                blur_new_optimizations = true

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
    nixosConfigurations = {
      prime-ai-bootstrap = nixpkgs-unstable.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.bootstrap_hardware
          self.nixosModules.system_config
          self.nixosModules.bootstrap_user
          inputs.ragenix.nixosModules.age
        ];
      };
      prime-ai = nixpkgs-unstable.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.prime-ai_hardware_config
          self.nixosModules.system_config
          self.nixosModules.phil_user
          self.nixosModules.wifi_secrets
          self.nixosModules.secure_boot
          self.nixosModules.prime-ai_hardware_shared_crypt
          inputs.ragenix.nixosModules.age
          self.nixosModules.prime_ai_tailscale
          self.nixosModules.network_fs
          self.nixosModules.hyprland-prime-ai
          #self.nixosModules.xfce_desktop
          self.nixosModules.phil_home
          self.nixosModules.spell_checkers
        ];
      };
    };
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
