{
  description = "Prime-AI Nixos Configuration";

  inputs  = {
    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-24.05";
    };
    
    nixpkgs-unstable = {
      # url = "github:PhDyellow/nixpkgs/master_patched";
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

    impermanence = {
      url = "github:nix-community/impermanence";
      # Don't pull in pinned nixpkgs or home-manager
      inputs.nixpkgs.follows = ""; 
      inputs.home-manager.follows = "";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    hyprland = {
      url = "github:hyprwm/Hyprland";
      # not following nixpkgs to get caching
    };

    nur = {
      url = "github:nix-community/NUR";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
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

    # TeX packages
    altacv = {
      url =  "github:Titan-C/AltaCV";
      flake = false;
    };
    moderncv = {
      url =  "github:Titan-C/moderncv";
      flake = false;
    };
    awesomecv = {
      url =  "github:posquit0/Awesome-CV";
      flake = false;
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
    org-fc = {
      url = "sourcehut:~l3kn/org-fc/main";
      flake = false;
    };
    org-cv = {
      url = "gitlab:Titan-C/org-cv";
      flake = false;
    };
    # Primarily for org-roam-review
    chrisbarrett-nursery = {
      url = "github:chrisbarrett/nursery/main";
      flake = false;
    };
    journalctl-el = {
      url = "github:WJCFerguson/journalctl/master";
      flake = false;
    };
    nix-on-droid = {
      url = "github:nix-community/nix-on-droid/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs-stable";
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

      flatpak = {
        base = import ./nixos-modules/flatpak/base.nix;
        impermanence = import ./nixos-modules/flatpak/impermanence.nix;
      };
      llms = {
        ollama = import ./nixos-modules/llms/ollama.nix;
        cudafy-ollama = import ./nixos-modules/llms/cudafy-ollama.nix;
        llama-cpp-cuda = import ./nixos-modules/llms/llama-cpp-cuda.nix;
      };
      system-conf = {
        localsend = import ./nixos-modules/system-conf/localsend.nix;
        network-printers = import ./nixos-modules/system-conf/network-printers.nix;
        network-scanners = import ./nixos-modules/system-conf/network-scanners.nix;
        umask = import ./nixos-modules/system-conf/umask.nix;
        # Programs that I want on all devices, and don't need to configure
        gui = import ./nixos-modules/system-conf/gui.nix;
        cli = import ./nixos-modules/system-conf/cli.nix;
        allow-unfree = import ./nixos-modules/system-conf/allow-unfree.nix;
        davfs = import ./nixos-modules/system-conf/davfs.nix;
        udisks = import ./nixos-modules/system-conf/udisks.nix;
        openssh = import ./nixos-modules/system-conf/openssh.nix;
        secure_boot = import ./nixos-modules/system-conf/secure-boot.nix;
        network_fs = import ./nixos-modules/system-conf/network-fs.nix;
        wifi_secrets = import ./nixos-modules/system-conf/wifi-secrets.nix;

        locale_au = import ./nixos-modules/system-conf/locale-au.nix;
        fonts = import ./nixos-modules/system-conf/fonts.nix;
        lock-root = import ./nixos-modules/system-conf/lock-root.nix;
        nix-config = import ./nixos-modules/system-conf/nix-config.nix;
        slurm-server = import ./nixos-modules/system-conf/slurm-server.nix;
      };

      ### Modules for specific machines

      nix-on-droid-config = {};

      server-vm-grass = {
        impermanence = import ./nixos-modules/server-vm-grass/impermanence.nix;
        networking = import ./nixos-modules/server-vm-grass/networking.nix;
      };

      x1carbon-vm = {
        vm-settings = import ./nixos-modules/x1carbon-vm/vm-settings.nix;
        boot = import ./nixos-modules/x1carbon-vm/boot.nix;
        networking = import ./nixos-modules/x1carbon-vm/networking.nix;
        trackpad = import ./nixos-modules/x1carbon-vm/trackpad.nix;
        desktop = import ./nixos-modules/x1carbon-vm/desktop.nix;
        fs = import ./nixos-modules/x1carbon-vm/fs.nix;
        phil_user = import ./nixos-modules/x1carbon-vm/phil-user.nix;
      };
      prime-ai = {
        gui = import ./nixos-modules/prime-ai/gui.nix;
        cli = import ./nixos-modules/prime-ai/cli.nix;

        networking = import ./nixos-modules/prime-ai/networking.nix;
        bluetooth = import ./nixos-modules/prime-ai/bluetooth.nix;
        hardware_config_tuxedo = import ./nixos-modules/prime-ai/hardware-config-tuxedo.nix;
        hardware_config = import ./nixos-modules/prime-ai/hardware-config.nix;
        impermanence-agenix = import ./nixos-modules/prime-ai/impermanence-agenix.nix;
        impermanence = import ./nixos-modules/prime-ai/impermanence.nix;
        hardware_shared_crypt = import ./nixos-modules/prime-ai/hardware-shared-crypt.nix;

        bootstrap_hardware = import ./nixos-modules/prime-ai/bootstrap-hardware.nix;
        bootstrap_user = import ./nixos-modules/prime-ai/bootstrap-user.nix;
        syncthing = import ./nixos-modules/prime-ai/syncthing.nix;
        tailscale = import ./nixos-modules/prime-ai/tailscale.nix;
        phil_user = import ./nixos-modules/prime-ai/phil-user.nix;

        phil_home = import ./nixos-modules/prime-ai/phil-home.nix;
      };



      #ssh_public_config

      window-managers = {
        sway = import ./nixos-modules/window-managers/sway.nix;
        sway-config-vm = import ./nixos-modules/window-managers/sway-config.nix;
        xfce_desktop = import ./nixos-modules/window-managers/xfce.nix;

        wayland-clipboard = import ./nixos-modules/window-managers/wayland-clipboard.nix;
        hyprland = import ./nixos-modules/window-managers/hyprland.nix;
      };
      cli = {
        ryzen-monitor = import ./nixos-modules/cli/ryzen-monitor.nix;

        # System wide TeX Live

        texlive-system = import ./nixos-modules/cli/texlive-system.nix;

        spell_checkers = import ./nixos-modules/cli/spell-checkers.nix;
        direnv = import ./nixos-modules/cli/direnv.nix;
      };

      gui = {
        inkscape = import ./nixos-modules/gui/inkscape.nix;
        nyxt-browser = import ./nixos-modules/gui/nyxt-browser.nix;
      };

      systemd-services = {
        ryzen-monitor-inital-ppt =  import ./nixos-modules/systemd-services/ryzen-monitor-initial-ppt.nix;
        bib_reorganise = import ./nixos-modules/systemd-services/bib-reorganise.nix;
      };

      nix-on-droid-modules = {
        phil-home = import ./nixos-modules/nix-on-droid-modules/phil-home.nix;
      };
    };

      #Modules for importing into home-manager.users.<name>.imports = [ here ];
    hmModules = {
      impermanence-phil = import ./hm-modules/impermanence-phil.nix;
      r-config = import ./hm-modules/r.nix;
      git-config = import ./hm-modules/git.nix;
      hyprland-config = import ./hm-modules/hyprland.nix;
      hyprlock-config = import ./hm-modules/hyprlock.nix;
      #
      gpg-agent-emacs = import ./hm-modules/gpg-agent.nix;
      emacs-hm-init = import ./hm-modules/emacs.nix;
      emacs-mwe = import ./hm-modules/emacs-min-working.nix;
      tex-full = import ./hm-modules/texlive-full.nix;
    };

    devShells."x86_64-linux" = {
      secureboot-tools = import ./dev-shells/secureboot-tools.nix;
    };

    packages."x86_64-linux" = {
    };

    overlays = {
      lsix_configured = import ./overlays/lsix-config.nix;

    };

    nixosConfigurations = {
      prime-ai-bootstrap = nixpkgs-unstable.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          self.nixosModules.prime-ai.bootstrap_hardware
          self.nixosModules.prime-ai.impermanence
          self.nixosModules.system-conf.lock-root
          self.nixosModules.system-conf.stateversion
          self.nixosModules.system-conf.nix-config
          self.nixosModules.prime-ai.bootstrap_user
        ];
      };
      prime-ai = nixpkgs-unstable.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos-modules/prime-ai/hardware-config.nix
          ./nixos-modules/prime-ai/networking.nix
          ./nixos-modules/prime-ai/bluetooth.nix
          ./nixos-modules/prime-ai/hardware-config-tuxedo.nix
          ./nixos-modules/prime-ai/hardware-shared-crypt.nix
          ./nixos-modules/prime-ai/impermanence.nix
          ./nixos-modules/prime-ai/impermanence-agenix.nix #not needed for bootstrap
          ./nixos-modules/prime-ai/syncthing.nix
          ./nixos-modules/prime-ai/tailscale.nix
          ./nixos-modules/prime-ai/phil-home.nix
          ./nixos-modules/prime-ai/phil-user.nix
          ./nixos-modules/prime-ai/gui.nix

          ./nixos-modules/system-conf/network-fs.nix
          ./nixos-modules/system-conf/wifi-secrets.nix
          ./nixos-modules/system-conf/secure-boot.nix

          
          ./nixos-modules/system-conf/localsend.nix
          ./nixos-modules/system-conf/openssh.nix
          ./nixos-modules/system-conf/allow-unfree.nix
          ./nixos-modules/system-conf/locale-au.nix
          ./nixos-modules/system-conf/umask.nix
          ./nixos-modules/system-conf/cli.nix
          ./nixos-modules/system-conf/gui.nix
          ./nixos-modules/system-conf/davfs.nix
          ./nixos-modules/system-conf/udisks.nix
          ./nixos-modules/system-conf/fonts.nix
          ./nixos-modules/system-conf/lock-root.nix
          ./nixos-modules/system-conf/nix-config.nix
          ./nixos-modules/system-conf/stateversion.nix
          ./nixos-modules/system-conf/slurm-server.nix
          ./nixos-modules/system-conf/network-printers.nix
          ./nixos-modules/system-conf/network-scanners.nix
          ./nixos-modules/window-managers/hyprland.nix
          ./nixos-modules/window-managers/wayland-clipboard.nix

          ./nixos-modules/flatpak/base.nix
          ./nixos-modules/flatpak/impermanence.nix

          # ./nixos-modules/bib_reorganise # riskier when using org-bibtex, may be editing notes when timer kicks in/
          ./nixos-modules/gui/inkscape.nix # works best when GTK is set up
          ./nixos-modules/gui/nyxt-browser.nix
          ./nixos-modules/cli/texlive-system.nix
          ./nixos-modules/cli/spell_checkers.nix
          ./nixos-modules/cli/direnv.nix
          #./nixos-modules/window-managers/xfce_desktop.nix

          ./nixos-modules/cli/ryzen-monitor.nix
          ./nixos-modules/systemd-services/ryzen-monitor-inital-ppt.nix

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
          self.nixosModules.system-conf.cli

          # self.nixosModules.window-managers.hyprland
          # self.nixosModules.window-managers.xfce_desktop
          self.nixosModules.window-managers.sway
          self.nixosModules.window-managers.sway-config-vm

          self.nixosModules.cli.spell_checkers
          self.nixosModules.cli.direnv

          self.nixosModules.system-conf.network_fs

          ## Need to get phil_home in here somehow
          self.nixosModules.prime-ai.phil_home

        ];
      };
      phil-vm-bootstrap = nixpkgs-unstable.lib.nixosSystem {
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
          # self.nixosModules.window-managers.xfce_desktop
          self.nixosModules.window-managers.sway
          self.nixosModules.window-managers.sway-config-vm

          self.nixosModules.cli.spell_checkers
          # self.nixosModules.cli.direnv

          self.nixosModules.system-conf.network_fs

          ## Need to get phil_home in here somehow
          # self.nixosModules.prime-ai.phil_home

        ];
      };


      server-vm-grass = nixpkgs-unstable.lib.nixosSystem {
        system = "x86_64-Linux";
        modules = [
          # Nix config
          self.nixosModules.system-conf.nix-config
          self.nixosModules.system-conf.stateversion-2605
          self.nixosModules.system-conf.allow-unfree

          # Impermanence



          # Hardware config

          # Folders and drives

          # SSH access
          self.nixosModules.system-conf.openssh

          # Networking
          self.nixosModules.server-vm-grass.networking

          # Admin user. maybe just with ssh keys?
          self.nixosModules.system-conf.lock-root

          # "Host" packages for system admin
          self.nixosModules.system-conf.locale_au

          # Containers and services




        ];
      };
    };
    nixOnDroidConfigurations = {
      # mostly just aiming to get emacs onto a phone here
      galaxym62 = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import inputs.nixpkgs-stable {system = "aarch64-linux";};
        modules = [
          self.nixosModules.system-conf.allow-unfree
          self.nixosModules.system-conf.locale_au
          self.nixosModules.system-conf.fonts
          self.nixosModules.system-conf.nix-config
          self.nixosModules.system-conf.stateversion-nix-on-droid
          self.nixosModules.system-conf.cli

          self.nixosModules.nix-on-droid-modules.phil-home

          self.nixosModules.cli.spell_checkers
          self.nixosModules.cli.direnv

        ];
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
