{config, pkgs, ...}:
{
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
        ## self.hmModules.emacs-mwe
        self.hmModules.gpg-agent-emacs
        ## self.hmModules.hyprland-config
        self.hmModules.git-config
        ##self.hmModules.r-config
        # self.hmModules.tex-full
        # self.hmModules.impermanence-phil
      ];
      manual.manpages.enable = true;

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
	  };
  };
}
