{
  description = "Prime-AI Nixos Configuration";

  inputs  = {
    nixpkgs-unstable = {
      #url = "github:NixOS/nixpkgs/nixos-unstable";
      url = "github:NixOS/nixpkgs/master"; #temporary change for bug in nixos
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
      url = github:nix-community/NUR;
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
    };
  };

  outputs = {self, nixpkgs-unstable, ...}@inputs: {
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
                  "offsetFanspeed": 37
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
            size = (1024 * 64) * 2; # Double RAM size. RHEL recommends 1.5x
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
              nvidiaPersistenced = true;
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
            nyxt
            st
            #agenix.packages.x86_64-linux.default #nix run github:ryantm/agenix -- --help
            python3
            openssl
              geekbench_6
            pqiv
            gthumb
          ];

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
      prime_overlays = {config, pkgs, ...}:
        {
          nixpkgs.overlays = [
            inputs.emacs-overlay.overlays.emacs
          ];

          nix.settings = {
            substituters = ["https://nix-community.cachix.org"];
            trusted-public-keys = ["nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="];
          };
        };
      phil_home = {config, pkgs, ...}: {
         imports = [
           inputs.home-manager.nixosModules.home-manager
        ];

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;

          #home-manager.users.<name> is an attribute set {} of users. Each user is a hmModule, so I can import
          #modules to it. Any modules imported by all users can go in home-manager.sharedModules
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

                    GITHUB_PAT = "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"

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
              extraConfig = ''
                  pinentry-program ${pkgs.pinentry-curses}/bin/pinentry-curses
                '';
              maxCacheTtl = 72000;

              };
              emacs = {
                enable = true;
                defaultEditor = true;
              };
            };

            programs = {
              emacs = {
                enable = true;
                package = pkgs.emacs;
                extraPackages = epkgs: [
                  #epkgs.vterm
                  #epkgs.eat

                ];
                overrides = final: prev: {

                };
                init = {
                  enable = true;
                  packageQuickstart = true;
                  recommendedGcSettings = true;
                  startupTimer = true;
                  earlyInit = "";
                  #config inserted before use-package
                  prelude = ''
                    ;;(setq my-user-emacs-directory "/storage/emulated/0/memx/repos/phone_emacs/")

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


                  '';
                  postlude = ""; #config inserted after use-package
                  #Packages configured
                  usePackage = {
                    frame = {
                      enable = true;
                      config = ''
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
                              #'command-completion-default-include-p)
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

                      '';
                    };
                    isend-mode = {
                      enable = true;
                    };
                    nix-mode = {
                      enable = false;
                    };
                    company-nixos-options = {
                      enable = false;
                    };
                    ob-nix = {
                      enable = false;
                    };
                    magit = {
                      enable = true;
                    };
                    forge = {
                      enable = true;
                      after = ["magit"];
                    };
                    vterm = {
                      enable = true;
                    };
                    eat = {
                      enable = true;
                    };
                    consult = {
                      after = ["xref"];
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
                    };
                    marginalia = {
                      enable = true;
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
                      config = ''
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
                                global-mark-ring
                                )
                              )
                        (setq history-length 250)
                        (savehist-mode)
                      '';
                    };
                    better-defaults = {
                      enable = true;
                    };
                    vertico = {
                      enable = true;
                      config = ''
                        (vertico-mode)
                      '';
                    };
                    ess = {
                      enable = true;
                      config = ''
                        ;;(setq inferior-ess-r-program "radian") ;;  ESS can't speak radian's language
                      '';
                    };
                    image-dired = {
                      enable = true;
                      config = ''
                        (setq image-dired-thumbnail-storage 'standard-large)
                      '';
                    };
                    "image-dired+" = {
                      enable = true;
                      config = ''
                        (image-diredx-async-mode 1)
                    '';
                    };
                    org = {
                      enable = true;
                      init = ''
                      '';
                      config = ''
                      (modify-all-frames-parameters '((inhibit-double-buffering . t)))
                      ;;Add R to org-babel
                        (org-babel-do-load-languages
                        'org-babel-load-languages
                        '((emacs-lisp . t)
                          (R . t)))

                          ;;Allow code blocks to execute without asking me every time
                          ;; for safetly though, don't allow C-c C-c to evaluate blocks
                          (setq org-confirm-babel-evaluate (lambda (lang src) (if (string= lang "R") nil t))
                                org-babel-no-eval-on-ctrl-c-ctrl-c t)
                      '';
                      demand = true;
                    };
                    zenburn-theme = {
                      enable = true;
                      init = ''
                        (setq zenburn-use-variable-pitch t)
                      '';
                      config = ''
                        (if (daemonp)
                          (add-hook 'after-make-frame-functions
                                    (lambda (frame)
                                      (select-frame frame)
                                      (load-theme 'zenburn t)
                                      (set-frame-font "DejaVu Sans 20" t t t)
                                    ;;(exwm-layout-set-fullscreen)
                        (menu-bar-mode -1)
                        (tool-bar-mode -1)
                        (scroll-bar-mode -1)
                        (fringe-mode 1)

                                      )
                                    )
                          (load-theme 'zenburn t)
                                      (set-frame-font "DejaVu Sans 20" t t t)
                          )
                      '';
                    };
                    browse-url = {
                      enable = true;
                      config = ''
                        (setq browse-url-generic-program "nyxt"
                          browse-url-browser-function 'browse-url-generic)
                      '';
                    };
                    which-key = {
                      enable = true;
                      config = ''
                        (which-key-mode)
                        '';
                    };
                    meow = {
                      enable = false;
                      config = ''
                        (require 'meow)
                        (meow-setup)
                        (meow-global-mode 1)
                        (setq meow-cursor-type-normal '(bar . 6))
                      '';
                      init = ''
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
                      '';
                  };
                };

              };
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
                    editor = "nvim";
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
   #enable_swallow = true
   swallow_regex = ^(Alacritty|kitty)$
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
bind = $mainMod, Q, exec, kitty
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
          #self.nixosModules.xfce_desktop
          self.nixosModules.phil_home
          self.nixosModules.prime_overlays
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
