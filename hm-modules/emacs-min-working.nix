{config, pkgs, ...}:
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
}
