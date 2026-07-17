{config, pkgs, ...}:
{
  nix.settings = {
    substituters = [
      "https://hyprland.cachix.org" # for hyprland
    ];
    trusted-public-keys = [
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" #for hyprland
    ];
  };
  nixpkgs.overlays = [
    self.overlays.lsix_configured
  ];

  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    portalPackage = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
  };

  programs.hyprlock = {
    enable = true;
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet -t -r -g 'Init: Prime-AI' --cmd start-hyprland";
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
      kitty
      foot
      lsix
      libsixel
      hyprshot
    ];
    sessionVariables = {
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
    hyprlock = {};
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
}
