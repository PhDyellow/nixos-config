{config, pkgs, ...}:
{
  home.persistence."/persistent" = {
    directories = [
      ".ssh"
      ".emacs.d"
      ".local/share/fish"
      ".cache/flatpak"
      ".local/share/flatpak"
    ];
    files = [
      ".config/enchant/en_AU.dic"
      ".config/enchant/en_AU.exc"
      ".config/nix/nix.conf"
    ];
  };

  # Create some directories on each boot, but do not persist them
  systemd.user = {
    enable = true;
    startServices = true;
    services.make-dirs = {
      Unit = {
        Description = "Creates some directories destroyed by  nixos impermanence";
        After = [ "graphical-session.target" ];
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = ''
            ${pkgs.coreutils}/bin/mkdir -p /home/${config.home.username}/Downloads
          '';
      };
    };
  };
}
