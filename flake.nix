{
  description = "Prime-AI Nixos Configuration";

  inputs  = {
    nixpkgs-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    ragenix = {
      url = "github:yaxitech/ragenix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs.nixpkgs.follows = "nixpkgs-unstable"; #needs unstable
    };

  };

  outputs = {self, nixpkgs-unstable, ...}@inputs:
    {
    nixosModules = {
      prime-ai_hardware_config = { config, lib, pkgs, modulesPath, ...}:
        {
          imports = [
              (modulesPath + "/installer/scan/not-detected.nix")
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
            cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
            video.hidpi.enable = lib.mkDefault true;
          };
        };
      prime-ai_hardware_shared_crypt = { config, lib, pkgs, ...}:
        {
          fileSystems = {
            "/para" = {
              device = "/dev/mapper/para-crypt"; #after mounting from crypttab
              fsType = "ntfs3";
              options = [ "ro"
                          "uid=1001"
                          "gid=100"
                          "windows_names"
                          "fmask=133"
                          "dmask=022"
                          "norecover"
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
            videoDrivers = ["nvidia"]; #noveau gave blank screen, nvidia needed nixpkgs.config.allowUnfree
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

          ];
        };
    };
  };
}
