{config, pkgs, ...}:
{
  environment.persistence."/persistent" = {
    directories = [
      "/var/lib/flatpak"
    ];
  };
}
