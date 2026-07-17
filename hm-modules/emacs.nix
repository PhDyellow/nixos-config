{config, pkgs, ...}:
{
  home.packages = with pkgs; [
    imagemagickBig
    librsvg
    inputs.pandoc-crossref.packages.x86_64-linux.pandoc-with-crossref
    pdf2svg
    graphviz-nox
  ];
  services = {
    emacs = {
      enable = false;
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
        epkgs.org-cv
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
        org-noter = prev.org-noter.overrideAttrs (old: {
          postInstall = (builtins.replaceStrings
            ["batch-native-compile"]
            ["ignore"]
            old.postInstall
          );
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
        # Org-fc should probably use a melpabuilder
        org-fc = prev.emacs.pkgs.trivialBuild {
          pname = "org-fc";
          version = "git";
          src = inputs.org-fc;
          packageRequires = [
            final.hydra
          ];
          preInstall = ''
                  LISPDIR=$out/share/emacs/site-lisp
                  mkdir -p $LISPDIR
                  cp -R awk $LISPDIR
                '';
        };
        org-cv = prev.emacs.pkgs.trivialBuild {
          pname = "org-cv";
          version = "git";
          src = inputs.org-cv;
          packageRequires = [
            final.ox-hugo
            final.dash
          ];
        };
        # trivialBuild assumes all list files are in the root dir
        # Using melpaBuild
        # melpaBuild is more complex than expected, trying
        # path concatenation
        chrisbarrett-nursery = prev.emacs.pkgs.trivialBuild {
          pname = "chrisbarrett-nursery";
          version = "git";

          src = inputs.chrisbarrett-nursery + "/lisp";

          packageRequires = [
            final.dash
            final.org-drill
            final.org-roam
            final.ts
            final.ht
            final.consult
            final.async
            final.f
            final.org-transclusion
            final.magit
            final.pcre2el
            final.memoize
          ];

        };
        journalctl = prev.emacs.pkgs.trivialBuild {
          pname = "journalctl";
          version = "git";
          src = inputs.journalctl-el;
          packageRequires = [
          ];
        };
      };
      init = {
        enable = true;
        ## TODO as of 2024-12-02 and emacs 31, package-quickstart breaks citar
        packageQuickstart = false;
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
                      (if  (getenv "TMPDIR")
                           nil
                           (setenv "TMPDIR" temporary-file-directory))



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
                       ;(citar-org-roam-mode)

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
          journalctl-mode = {
            enable = false;
          };
          journalctl = {
            enable = true;
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
            # use hs-mode, which is built in and works better for nix
            enable = false;
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

                          ; defined in `simple`
                          ;(setq read-extended-command-predicate
;#'command-completion-default-include-p)
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

                          (set-default 'truncate-lines t)

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
          ol-git-link = {
            enable = true;
            package = "org-contrib";
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
                          ;; (setq consult-preview-key (kbd "M-]"))
                          (setq consult-preview-key (list "M-]"))
                          ;; For some commands and buffer sources it is useful to configure the
                          ;; :preview-key on a per-command basis using the `consult-customize' macro.
                          (consult-customize
                          consult-theme
                          consult-buffer
                          consult-ripgrep
                          consult-git-grep
                          consult-grep
                          consult-bookmark
                          consult-fd
                          consult-locate
                          consult-recent-file
                          consult-xref
                          consult--source-bookmark
                          consult--source-file-register
                          consult--source-recent-file
                          consult--source-project-recent-file
                          consult-notes
                          consult-dir
                          consult-recoll
                          consult-org
                          )

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
                              flycheck-idle-change-delay 5
                              ;; org-lint is slow in the memx, and causes frequent multi-second hangs
                              flycheck-disabled-checkers '(org-lint))
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
                          completion-pcm-leading-wildcard t
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
            init = ''
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
          vertico-sort = {
            enable = true;
            after = [ "vertico" ];
            config = ''
                    ;; this should have been true by default
                    ;; but was failing for reasons I don't understand.
                    ;; Setting it does solve my problem.
                    (setq vertico-sort-function #'vertico-sort-history-length-alpha)
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
          picpocket = {
            enable = false; # thumbnail handling wasn't better than image-dired
          };
          dirvish = {
            enable = true;
            extraPackages = with pkgs; [ ];
          };
          # work around bug in image-converter when using imagemagick v7
          # and opening .heic files
          image-converter = {
            enable = true;
            config = ''
                    (setq image-use-external-converter t)
                    (set 'image-converter--converters
                      '((graphicsmagick :command ("gm" "convert") :probe ("-list" "format"))
                       (ffmpeg :command "ffmpeg" :probe "-decoders")
                        (imagemagick :command "magick" :probe ("-list" "format"))))
                  '';
          };
          image-dired = {
            enable = true;
            config = ''
                          (setq image-dired-thumbnail-storage 'standard-large)
                        '';
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
          corfu-history = {
            enable = true;
            after = [ "corfu" ];
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
          ## org-cv packages
          org-cv = {
            enable = false;
          };
          ox-altacv = {
            enable = true;
          };
          ox-awesomecv = {
            enable = true;
          };
          ox-awesomecv2 = {
            enable = true;
          };
          ox-hugocv = {
            enable = true;
          };
          ox-moderncv = {
            enable = true;
          };
          ox-hugo = {
            enable = true;
            after = [ "ox" ];
          };
          org-drill = {
            enable = false;
            config = ''
                    (setq org-drill-scope 'directory
                          org-drill-add-random-noise-to-intervals-p t
                          org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
                          '';
          };
          org-roam-review = {
            enable = true;
            package = "chrisbarrett-nursery";
            config = ''
                    ;; org-roam-review assumes notes are file notes
                    ;; below is a modified org-roam-review-list-due
                    ;; function that also allows heading notes

(defun org-roam-review-list-due ()
  "List nodes that are due for review."
  (interactive)
  (display-buffer
   (org-roam-review-create-buffer
    :title "Due Notes"
    :instructions "The nodes below are due for review.
Read each node and add new thoughts and connections, then mark
them as reviewed with `org-roam-review-accept',
`org-roam-review-bury' or by updating their maturity."
    :placeholder (concat (propertize "You're up-to-date!" 'face 'font-lock-comment-face) " 😸")
    :group-on #'org-roam-review--maturity-header
    :sort (-on #'ts< #'org-roam-review-node-next-review)
    :nodes
    (lambda ()
      (seq-filter (lambda (node)
                    (and (null (seq-intersection (org-roam-node-tags node)
                                                 org-roam-review-tags-ignored-for-review-buffer))
                         (org-roam-review-node-due-p node)))
                  (org-roam-review-node-list))))))
                  '';
          };
          hydra = {
            enable = true;
          };
          org-fc = {
            enable = true;
            after = [ "hydra" ];
            config = ''
                    (require 'org-fc-hydra)
                  '';
            extraConfig = ''
                    :custom (org-fc-directories `(,my-memx-dir))
                  '';
          };
          org = {
            enable = true;
            demand = true;
            init = ''
                        '';
            config = ''
                          ;; Don't hide links. Uglier, but easier to manipulate
                          (setq org-link-descriptive nil)

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
                            (sequence "ROUTINE(o)" "|" "DROPPED(!)") ; A routine activity that repeats regularly. Should have justification, activity and active timestamp
                            (sequence "GOAL(g)" "|" "ACHIEVED(!)") ;An outcome I want to see come about, ideally with a deadline using SMART principles
                            ;(sequence "PROJ(p)" "|" "COMPLETE(!)") ;A set of tasks supporting a goal or higher project. Make redundant
                            (sequence "TASK(t)" "|" "DONE(!)") ; A task. Tasks should pursue goals. A group of related tasks are really just subtasks, no projects.
                            (sequence "SPARK(s)" "THINKING(n)" "|" "PROCESSED(!)") ;Transient notes that I want to capture
                            (sequence "READ(r)" "|" "CITE(c)") ;bibliographic entries. Switch back and forth as needed
                            (sequence "|" "CONCEPT(w)" "VIEW(v)" "JOURNAL(j)") ;evergreen concept note, overview of area, fleeting note
                            ;(sequence "PROBLEM" "|" "SOLVED" "SKIP")
                            ;(sequence "ATTEMPT" "|" "FAIL" "SUCCESS")
                            ;(sequence "BLOCKED(b)" "TODO(t)" "NEXT(n!)" "WAIT(b@/!)" "IDEA(i@)" "|" "NEVER(x@)" "DONE(d!)")
                            ;(sequence "ADD" "FIND" "SKIM(1!)" "1ST_READ(2!)" "1ST_MARG(3!)" "2ND_READ(4!)" "2ND_MARG(5!)" "3RD_READ(6!)" "3RD_MARG(7!)" "GOLD(8!)" "|" "REF(9!)" "DROP(k)")
                          ))

                          ;; set up Priorities that map to emotional capacity
                          ;; see "energy and emotional capacity based priority system for org-mode, my todo system, memx, anchoring system"
                          ;; for details
                          (setq org-priority-highest 1
                                org-priority-lowest 11
                                org-priority-default 7)


                          (setq org-tag-alist '(
                          ;; (startgroup . nil) ... (endgroup . nil) -> Mutually exclusive  tags
                          ;; (startgroup . nil) (tagggx . "t") (grouptags . nil) ... (endgroup . nil) -> Mutually exclusive tags in tag group "tagggx"
                          ;; (startgrouptag . nil) (taggg . "t") (grouptags . nil) ... (endgrouptag . nil) ->  tag group "taggg", not exclusive
                          ;; regular tag "proj"
                          ;; regexp tag "{proj_.+}" -> matches any tag starting with "proj_". Useful as part of group tag, eg ((startgroup) ("proj") (grouptags) ("{proj_.+}")(endgroup)) which allows searches for "proj" to match all projects, or proj_win to just match the proj_win project.
                          ;; If a tag is mutually exclusive with PROCESSING, then the tag is always a processing tag.

                          ;;
                          (:startgroup)
                              ("TYPE" . ?1)
                              (:grouptags)
                                  ("PROCESS" . ?2)
                                             ("PROCESSING" . ?4)
                                             ("PROCESSED" . ?5)
                                  ("INFORMATION" . ?3)
                                             ("CURRENT" . ?6)
                                             ("STALE" . ?7)
                          (:endgroup)
                          (:startgroup)
                              ("PROCESS" . ?2)
                              (:grouptags)
                                  ("PROCESSING" . ?4)
                                  ("PROCESSED" . ?5)
                          (:endgroup)
                          (:startgroup)
                              ("INFORMATION" . ?3)
                              (:grouptags)
                                  ("CURRENT" . ?6)
                                  ("STALE" . ?7)
                          (:endgroup)
                          (:startgroup)
                              ("PROCESSING" . ?4)
                              (:grouptags)
;; QUERY tag for question, investigation log, conclusion.
                                  ("QUERY" . ?u)
;; SPARK tag for an idea, probably crossing together a lot of other ideas and sources. XXSPARK is a spark that has been processed
                                  ("SPARK" . ?s)
;; PROJ tag for a goal with tasks. Use PROJ if it is something that can be marked as done, is more complex than a single task and achieves something in itself when done (ie. not just partway to a desired state. A counterexample: submitting an application then accepting the returned offer are two tasks, the first one is not a complete project on it's own.). A subproject is also a PROJ, linked to the parent proj in a super/sub type link pair.
                                  ("PROJ" . ?p)
;; TASK tag for a task. Often redundant due to TODO states.
                                  ("TASK" . ?t)
;; AREA tag for things I am responsible for, and want to maintain, but cannot be marked as done. I'm never really "done" with being a parent or husband, for example, or taking care of my health. Areas do close though, as life circumstances change.
                                  ("AREA" . ?a)
;; GOAL tag for things I want to work towards. Goals are visions of the future. An active goal is defined by a clear outcome. Goals pursue values, values inform goals.
                                  ("GOAL" . ?g)
;; WORKS tag for things I am working on. Code, writing, in collaboration, solo... They are marked as closed when I don't plan on changing anything else in them.
                                  ("ARTEFACT" . ?w)
;; THINKING tag, for thinking things through. Like SPARK, but when I dont have an idea.
                                  ("THINKING" . ?h)
                          (:endgroup)
                          (:startgroup)
                              ("PROCESSED" . ?5)
                              (:grouptags)
                                  ("XXQUERY" . ?U)
                                  ("XXSPARK" . ?S)
                                  ("XXPROJ" . ?P)
                                  ("XXTASK" . ?T)
                                  ("XXAREA" . ?A)
                                  ("XXGOAL" . ?G)
                                  ("XXARTEFACT" . ?W)
                                  ("XXTHINKING" . ?H)
                          (:endgroup)
                          (:startgroup)
                              ("CURRENT" . ?6)
                              (:grouptags)
;; RECIPE tag for how to do something
                                  ("RECIPE" . ?r)
;; CITE tag for a reference or source. XXCITE is a retracted source
                                  ("CITE" . ?c)
;; VALUE tag for my fundamental understanding of things that are important. Values are concepts. Values are used to discern between actions that all seem good.
                                  ("VALUE" . ?v)
;; CONCEPT tag for notes that explain a concept. Theseare the "evergreen" notes in my memx, and should be reasonably self-contained and understandable to an outsider. Concept notes are not normally closed, but they might be retracted.
                                  ("CONCEPT" . ?o)
;; ENTITY  tag, for a thing, like a person, company, city or software program. Entity tags are closed when I no longer want to see them, not when the entity ceases to exist, as some entities are historical or fictional.
                                  ("ENTITY" . ?e)
;; Contains TOC, org-ql-views, overview, or some kind of summary that mostly just links to other things.
                                  ("VIEW" . ?b)
                          (:endgroup)
                          (:startgroup)
                              ("STALE" . ?7)
                              (:grouptags)
                                  ("XXRECIPE" . ?R)
                                  ("XXCITE" . ?C)
                                  ("XXVALUE" . ?V)
                                  ("XXCONCEPT" . ?O)
                                  ("XXENTITY" . ?E)
                                  ("XXVIEW" . ?B)
                          (:endgroup)))



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
          # Org-babel
          ob = {
            enable = true;
            config = ''
                         ;; configure allowed languages
                         (setq org-babel-load-languages
                                '((R . t)
                                 (emacs-lisp . t)
                                 (python . t)
                                 (plantuml . t)
                                 (latex . t)
                                 (d2 . t)
                                 (julia . t)
                                 (sql . t)
                                 (sqlite . t)))

                         (org-babel-do-load-languages
                            'org-babel-load-languages
                            org-babel-load-languages)
                  '';
          };
          ob-nix = {
            enable = true;
          };
          ob-chatgpt-shell = {
            after = ["gptel"];
            enable = true;
            extraConfig = ''
                    :custom
                      ((chatgpt-shell-openai-key
                      (lambda ()
                      (gptel-api-key-from-auth-source "api.openai.com" "apikey"))))
                   '';
          };
          ob-d2 = {
            enable = true;
            config = ''
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
          ob-plantuml = {
            enable = true;
            config = ''
                        (setq org-plantuml-exec-mode 'plantuml)
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

                        (citar-org-roam-mode)
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
          };
          pdf-tools = {
            enable = true;
            extraPackages = with pkgs; [ ghostscript ];
            config = ''
(let* ((files (directory-files (file-name-directory
																(locate-library "pdf-tools")) nil "\\.el$"))
	     (names (remove "pdf-tools-pkg" (seq-map #'(lambda (s) (string-trim-right s "\\.el")) files)))
	     (symbols (seq-map #'intern names)))
  (seq-do #'require symbols))

                        (pdf-loader-install)
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
            # mode = [ ''
            #        ("\\.Rd\\'" . Rd-mode)
            #        ("DESCRIPTION\\'" . conf-colon-mode)
            #        ("\\.Rd\\'" . Rd-mode)
            #        ("DESCRIPTION\\'" . conf-colon-mode)
            #        ("\\.[Rr]out\\'" . ess-r-transcript-mode)
            #        ("CITATION\\'" . ess-r-mode)
            #        ("NAMESPACE\\'" . ess-r-mode)
            #        ("\\.[rR]profile\\'" . ess-r-mode)
            #        ("\\.[rR]\\'" . ess-r-mode)
            #        ("/R/.*\\.q\\'" . ess-r-mode)
            #        ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
            #        ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
            #        ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
            #        ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
            #      '' ];
            config = ''
                            '';
          };
          ess = {
            enable = true;
            init = ''(require 'ess-site)'';
            config = ''
                          ;;(setq inferior-ess-r-program "radian") ;;  ESS can't speak radian's language
                          (setq ess-use-flymake nil)
                        '';
            extraConfig = ''
                          :interpreter (("r" . ess-r-mode)
                          ("Rscript" . ess-r-mode))
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
          plantuml-mode = {
            enable = true;
            after = [ "org" ];
            config = ''
                        (setq plantuml-default-exec-mode 'executable)
                   '';
          };
          python-mode = {
            enable = true;
            after = [ "org" ];
          };
          flycheck-plantuml = {
            enable = true;
            after = [ "flycheck" "plantuml-mode" ];
            config = ''
                        (flycheck-plantuml-setup)
                      '';
          };

          chatgpt-shell = {
            after = ["gptel"];
            enable = true;
            extraConfig = ''
                    :custom
                      ((chatgpt-shell-openai-key
                      (lambda ()
                      (gptel-api-key-from-auth-source "api.openai.com" "apikey"))))

                   '';
          };
          gptel = {
            enable = true;
            config = ''
                  (require 'gptel-org)
                  (require 'gptel-openai)
                  (require 'gptel-openai-extras)
                  (require 'gptel-rewrite)
                  (require 'gptel-transient)
                  (require 'gptel-context)
                  (require 'gptel-ollama)
                  ;; OpenRouter offers an OpenAI compatible API
                  (setq gptel-model 'meta-llama/llama-3.3-70b-instruct:free)
                  (setq gptel-backend
                    (gptel-make-openai "OpenRouter"              ;Any name you want
                    :host "openrouter.ai"
                    :endpoint "/api/v1/chat/completions"
                    :stream t
                    :key (lambda ()
                      (gptel-api-key-from-auth-source "api.openrouter.com" "apikey") )
                    :models '(cognitivecomputations/dolphin3.0-r1-mistral-24b:free
                              deepseek/deepseek-chat:free
                              deepseek/deepseek-r1:free
                              meta-llama/llama-3.3-70b-instruct:free
                    )))
                  '';
          };
          evedel = {
            enable = true;
            config = ''
                  (require 'evedel-gptel)
                  (require 'evedel-instructions)
                  (require 'evedel-restorer)
                  (require 'evedel-utilities)
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
}
