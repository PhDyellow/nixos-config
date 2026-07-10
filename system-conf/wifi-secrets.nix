{config, pkgs, ...}:
{
  age.secrets.wpa_pwd_env = {
    file = ./agenix/wpa_pwd.env.age;
    mode = "600";
    owner = "wpa_supplicant";
    group = "wpa_supplicant";
  };
  networking.wireless = {
    secretsFile = config.age.secrets.wpa_pwd_env.path;
    networks = {
      PBAGJmob = {
        pskRaw = "ext:phone_psk";
        priority = 20;
      };
      WIFI-56E0-5G = {
        pskRaw = "ext:parent_psk";
        priority = 60;
      };
      WiFi-56E0-5G = {
        pskRaw = "ext:parent_psk";
        priority = 65;
      };
      PBAGJE_H_5G = {
        pskRaw = "ext:home_psk";
        priority = 99;
      };
      #BParent 2.4Ghz
      Telstra6C9EC9 = {
        pskRaw = "ext:bparent_psk";
        priority = 50;
      };
    };
  };
}
