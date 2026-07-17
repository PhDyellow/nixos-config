{config, pkgs, ...}:
let
  # this line prevents hanging on network split
  automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=600,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,uid=1001,gid=100";
in
{
  age.secrets.cifs_dpbagje_share.file = ./agenix/cifs_dpbagje_share.age;
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
    "/nas/dpbagj/repos" = {
      device = "//100.108.81.63/repos";
      fsType = "cifs";
      options = ["${automount_opts},credentials=${config.age.secrets.cifs_dpbagje_share.path}"];
    };
  };
}
