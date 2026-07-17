{ config, lib, pkgs, ...}:
{
  fileSystems = {
    #tested ntfs-3g and ntfs3 with
    #dd if=/dev/urandom of=/para/test.bad oflag=direct count=32k bs=128k
    #ntfs-3g (472, 448, 469)Mb/s
    #ntfs3 (323, 324, 329)Mb/r
    #ntfs-3g is faster!
    "/para" = {
      device = "/dev/mapper/para-crypt"; #after mounting from crypttab
      fsType = "btrfs";
      options = [
        "noacl" # single user system
        "compress=zstd:8" # moderately aggressive compression
        "space_cache=v2"
        "discard=async" # Use trim occasionally
        "max_inline=28k" # 28k files are inlined.
        #I configured 32k nodes, so this is not too large
        "ssd" # force SSD
        "noatime" #reduce writes by not updating atimes on each read
        "nofail"
        #"discard" #ntfs3 only option
      ];
    };
  };
  environment.etc.crypttab = {
    enable = true;
    text = ''
            # para-crypt /dev/disk/by-partuuid/1b5333c3-9421-44d5-8d21-fc2f22c8cbe3 /secrets/bitlocker/para.bek bitlk
            para-crypt /dev/disk/by-partlabel/PARACRYPT /secrets/bitlocker/para.bek bitlk,nofail
          '';
  };
}
