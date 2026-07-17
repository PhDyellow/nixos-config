{config, pkgs, ...}:
{
  imports = [
    inputs.hyprland.homeManagerModules.default
    self.nixosModules.hmModules.hyprlock
  ];
  home = {
    pointerCursor = {
      x11.enable = true;
      gtk.enable = true;
      package = pkgs.catppuccin-cursors.frappeYellow;
      name = "catppuccin-frappe-yellow-cursors";
      size = 24;
    };
    sessionVariables = {
      GTK_THEME = "Andromeda";
    };
  };
  # See https://hoverbear.org/blog/declarative-gnome-configuration-in-nixos/
  gtk = {
    enable = true;
    theme = {
      package = pkgs.andromeda-gtk-theme;
      name = "Andromeda";
    };
    iconTheme = {
      package = pkgs.andromeda-gtk-theme;
      name = "Andromeda";
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
  };
  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = true;
    settings = {

      # settings for NVIDIA
      env = [
        "LIBVA_DRIVER_NAME,nvidia"
        "XDG_SESSION_TYPE,wayland"
        "GBM_BACKEND,nvidia-drm"
        "__GLX_VENDOR_LIBRARY_NAME,nvidia"
        "NVD_BACKEND,direct"
        "NIXOS_OZONE_WL,1"

        "HYPRCURSOR_THEME,catppuccin-frappe-yellow-cursors"
        "HYPRCURSOR_SIZE,24"
      ];

      monitor="eDP-1,2560x1440@165,0x0,1";
      exec-once = "emacs";
      input = {
        kb_layout = "us";
        kb_variant = "";
        kb_model = "";
        kb_options = "";
        kb_rules = "";

        follow_mouse = 2;

        sensitivity = 0; # -1.0 - 1.0, 0 means no modification.

        touchpad = {
          natural_scroll = false;
        };
      };
      general = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more
        gaps_in = 5;
        gaps_out = 20;
        border_size = 2;
        "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
        "col.inactive_border" = "rgba(595959aa)";
        layout = "dwindle";
      };
      decoration = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more
        rounding = 10;
        blur = {
          enabled = true;
          size = 3;
          passes = 1;
          new_optimizations = true;
        };
        shadow = {
          enabled = true;
          range = 4;
          render_power = 3;
          color = "rgba(1a1a1aee)";
        };
      };
      animations = {
        enabled = true;

        # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

        bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";

        animation = [
          "windows, 1, 7, myBezier"
          "windowsOut, 1, 7, default, popin 80%"
          "border, 1, 10, default"
          "borderangle, 1, 8, default"
          "fade, 1, 7, default"
          "workspaces, 1, 6, default"
        ];

      };

      misc = {
        enable_swallow = true;
        swallow_regex = "^(Alacritty|kitty|foot)$";
        force_default_wallpaper = 0;
      };
      # trigger when the switch is toggled
      bindl= [
        ",switch:on:Lid Switch,exec,hyprlock"
      ];
      # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more

      dwindle = {
        # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
        pseudotile = true; # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
        preserve_split = true; # you probably want this
      };

      "$mainMod" = "SUPER";

      bindm = [
        # Move/resize windows with mainMod + LMB/RMB and dragging
        "$mainMod, mouse:272, movewindow"
        "$mainMod, mouse:273, resizewindow"
      ];

      bind = [
        "$mainMod, E, exec, emacs"
        "$mainMod, Q, exec, foot"
        "$mainMod, C, killactive,"
        "$mainMod, M, exit,"
        "$mainMod, L, exec, hyprlock"
        "$mainMod, V, togglefloating,"
        ##"$mainMod, R, exec, wofi --show drun"
        "$mainMod, P, pseudo," # dwindle
        "$mainMod, J, layoutmsg, togglesplit" # dwindle


        # Move focus with mainMod + arrow keys
        "$mainMod, left, movefocus, l"
        "$mainMod, right, movefocus, r"
        "$mainMod, up, movefocus, u"
        "$mainMod, down, movefocus, d"

      ]
      ++ (
        # workspaces
        # binds $mainMod + [shift +] {1..10} to [move to] workspace {1..10}
        builtins.concatLists (
          builtins.genList (
            x: let ws =
                     let
                       c = (x + 1) / 10;
                     in
                       builtins.toString (x + 1 - (c * 10));
               in
                 [
                   "$mainMod, ${ws}, workspace, ${toString (x + 1)}"
                   "$mainMod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}"
                 ]
          ) 10
        )
      );
    };
    plugins = [
    ];
  };


}
