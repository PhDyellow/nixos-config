{config, lib, pkgs, ...}:
{
  imports = [
    inputs.impermanence.nixosModules.impermanence
  ];

  environment.persistence."/persistent" = {
    enable = true;
    hideMounts = true;
    directories = [
      # This will grow as I discover things
      # Persistence is for things that either
      # 1. Must not go in the nix store
      # 2. Are frequently updated by the application
      # examples of 2. are cache files and data, but
      # where possible these should be configured to go elsewhere
      {
        directory = "/secrets"; # syncthing is here too
        mode = "0700";
        user = "root";
        group = "root";
      }
      "/etc/secureboot"
      "/var/lib/nixos" # persist random nixos serice UIDs
      "/var/lib/systemd"
      "/var/lib/syncthing"
      "/var/lib/tailscale"
      "/var/lib/bluetooth"
      "/nix"
      "/var/log"
      # quick note, these are mounted from /persistent/{dir} to /{dir}
      # That may be necessary knowledge for initial install
    ];

    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
    ];
  };
}
