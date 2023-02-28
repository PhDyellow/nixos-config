{
  description = "Prime-AI Nixos Configuration";

  inputs  = {
    nixpkgs-unstable = {
      #url = "github:NixOS/nixpkgs/nixos-unstable";
      url = "github:Kiskae/nixpkgs/nvidia/unbreak-6.2";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    ragenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs.nixpkgs.follows = "nixpkgs-unstable"; #needs unstable
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
      #not following nixpkgs to get caching
    };
  };

  outputs = {self, nixpkgs-unstable, ...}@inputs:
    {
    nixosModules = {
      prime-ai_hardware_config = { config, lib, pkgs, modulesPath, ...}:
        {
          imports = [
              (modulesPath + "/installer/scan/not-detected.nix")
              inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
              inputs.nixos-hardware.nixosModules.common-cpu-amd
              inputs.nixos-hardware.nixosModules.common-pc-ssd
              inputs.nixos-hardware.nixosModules.common-pc
              inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
          ];

          boot = {
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
            ];
            extraModulePackages = [ ];
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
          swapDevices = [ ];
          hardware = {
            video.hidpi.enable = lib.mkDefault true;
          };

          environment.systemPackages = with pkgs; [
            zenstates
            ryzenadj
            linuxPackages.zenpower
            zenmonitor
            #amdctl #not in nixos, but does same job as zenstates
          ];
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

          services.xserver = {
            enable = true;
            displayManager.lightdm.enable = true;
            displayManager.defaultSession = "none+dwm";
            windowManager.dwm.enable = true;
          };

          sound.enable = true;
          hardware.pulseaudio.enable = true;

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
            st
            #agenix.packages.x86_64-linux.default #nix run github:ryantm/agenix -- --help
            python3
            openssl
          ];

          nix = {
            package = pkgs.nixVersions.unstable;
            settings = {
              system-features = [
                "recursive-nix"
                "kvm"
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

          programs.hyprland = {
            enable = true;
            nvidiaPatches = true;
          };

          environment = {

            systemPackages = with pkgs; [
              kitty
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
          # obviously
          services.xserver.videoDrivers = ["nvidia"];

          hardware = {
            nvidia = {
              open = true;
              powerManagement.enable = true;
              modesetting.enable = true;
            };

            opengl = {
              enable = true;
              driSupport = true;
              driSupport32Bit = true;
              extraPackages = with pkgs; [
                vaapiVdpau
                libvdpau-va-gl
                nvidia-vaapi-driver
              ];
            };
            pulseaudio.support32Bit = true;
          };
          nix.settings = {
            substituters = ["https://hyprland.cachix.org"];
            trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
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
            self.nixosModules.prime-ai_hardware_config
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
          ];
        };
    };
  };
}
