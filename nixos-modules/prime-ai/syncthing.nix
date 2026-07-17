{config, pkgs, ...}:
{
  services.syncthing = {
    enable = true;
    dataDir = "/var/lib/syncthing"; # default, also persisted
    overrideDevices = true;
    overrideFolders = true;
    user = "phil";
    group = "users";
    openDefaultPorts = true;
    settings = {
      devices = {
        dpbagje = {
          addresses = [
            "tcp://192.168.20.10:22000"
            "tcp://dpbagje.philjd.com:22000"
            "quic://192.168.20.10:22000"
            "quic://dpbagje.philjd.com:22000"
            "dynamic"
          ];
          id = "V2CZC46-XWNXBME-WDTOBSM-SHIO25H-KTTUFYQ-BXRRXFM-PQFYLYK-LUGCBQK";
        };
        galaxy_m62 = {
          addresses = [
            "tcp://100.89.185.54:22000"
            "tcp://192.168.20.14:22000"
            "tcp://192.168.1.119:22000"
            "quic://100.89.185.54:22000"
            "quic://192.168.20.14:22000"
            "quic://192.168.1.119:22000"
            "dynamic"
          ];
          id = "O4OCDD3-BN3WGHU-4U42GOR-CZQQTSQ-GTSULNM-YQM76V5-6R7RT2Y-TTOG5AG";
        };
        x1_carbon = {
          addresses = [
            "tcp://100.103.6.30:2200"
            "quic://100.103.6.30:2200"
            "dynamic"
          ];
          id = "PBQHAZ3-VEXG3K6-VC7AHMS-3OPLJOJ-SIL4UFP-MRIPZHL-PS2DUTD-DO6QXA6";
        };
      };
      folders = {
        tagpaths = {
          path = "/para/tagpaths/";
          id = "hnris-vatpw";
          enable = true;
          devices = [
            "dpbagje"
            "galaxy_m62"
            "x1_carbon"
          ];
        };
        memx = {
          path = "/para/areas/memx___syncthing/";
          id = "nihsu-jd7zf";
          enable = true;
          devices = [
            "dpbagje"
            "galaxy_m62"
            "x1_carbon"
          ];
        };
        memx_transition = {
          path = "/para/resources/memx___syncthing__transition";
          id = "raehb-7gn4q";
          enable = true;
          devices = [
            "dpbagje"
            "galaxy_m62"
            "x1_carbon"
          ];
        };
        manage_time_transition = {
          path = "/para/resources/manage_time___syncthing__transition";
          id = "tg7ol-vf4xc";
          enable = true;
          devices = [
            "dpbagje"
            "galaxy_m62"
            "x1_carbon"
          ];
        };
        transfer_sync = {
          path = "/para/resources/transfer_sync___syncthing";
          id = "0nae2-zo3f7";
          enable = true;
          devices = [
            "dpbagje"
            "galaxy_m62"
            "x1_carbon"
          ];
        };
        reading_transition = {
          path = "/para/resources/reading___syncthing__transition/";
          id = "sy3q4-6cput";
          enable = true;
          devices = [
            "dpbagje"
            "x1_carbon"
          ];
        };
        zettlekasten_transition = {
          path = "/para/resources/zettlekasten___syncthing__transition/";
          id = "wsbyx-rus2l";
          enable = true;
          devices = [
            "dpbagje"
            "x1_carbon"
          ];
        };
        zotfile_storage_transition = {
          path = "/para/resources/zotfile_storage___syncthing__transition/";
          id = "jzunu-qsesd";
          enable = true;
          devices = [
            "dpbagje"
            "x1_carbon"
          ];
        };
        phd_transition = {
          path = "/para/projects/phd___syncthing__transition/";
          id = "nbdjg-farns";
          enable = true;
          devices = [
            "x1_carbon"
          ];
        };
        phd_draft = {
          path = "/para/projects/phd_draft___syncthing/";
          id = "ceto7-fdkqr";
          enable = true;
          devices = [
            "x1_carbon"
          ];
        };
        synology_nas_admin = {
          path = "/para/projects/synology_nas_admin___syncthing/";
          id = "rfra6-xulze";
          enable = true;
          devices = [
            "x1_carbon"
            "dpbagje"
          ];
        };
      };
    };
    cert = "/secrets/syncthing/cert.pem";
    key = "/secrets/syncthing/key.pem";
  };
}
