{ config, lib, pkgs, modulesPath, ...}:
let
  tailor-super-fans = pkgs.writeTextFile {
    name = "super-fans.json";
    text = ''
                [
                  {"temp":20,"fan":0},
                  {"temp":80,"fan":100},
                  {"temp":100,"fan":100}
                ]
              '';
  };
  tailor-super-profiles = pkgs.writeTextFile {
    name = "super.json";
    text = ''
                {
                  "fans":["super-fans","super-fans"],
                  "leds":[],
                  "performance_profile":"performance"
                }
              '';
  };
  tailor-super-activate = pkgs.writeTextFile {
    name = "active_profile.json";
    text = tailor-super-profiles.text;
  };
  tcc-profile = pkgs.writeTextFile {
    name = "tcc-profile";
    text =  ''
              [
                {
                  "id": "__default_custom_profile__",
                  "name": "TUXEDO Defaults",
                  "description": "Edit profile to change behaviour",
                  "display": {
                    "brightness": 100,
                    "useBrightness": false
                  },
                  "cpu": {
                         "useMaxPerfGov": false,
                         "governor": "performance",
                         "energyPerformancePreference": "performance",
                         "noTurbo": false,
                         "onlineCores": 24,
                         "scalingMinFrequency": 500000,
                         "scalingMaxFrequency": 4900000
                  },
                  "webcam": {
                    "status": true,
                    "useStatus": true
                  },
                  "fan": {
                    "useControl": true,
                    "fanProfile": "Balanced",
                    "minimumFanspeed": 0,
                    "offsetFanspeed": 0
                  },
                  "odmProfile": {
                    "name": "performance"
                  },
                  "odmPowerLimits": {
                    "tdpValues": []
                  }
                },
              {
                  "name": "freezy",
                  "description": "Edit profile to change behaviour",
                  "display": {
                    "brightness": 5,
                    "useBrightness": true
                  },
                  "cpu": {
                    "useMaxPerfGov": false,
                    "governor": "powersave",
                    "energyPerformancePreference": "performance",
                    "noTurbo": false,
                    "onlineCores": 24,
                    "scalingMinFrequency": 550000,
                    "scalingMaxFrequency": 5074000
                  },
                  "webcam": {
                    "status": false,
                    "useStatus": true
                  },
                  "fan": {
                    "useControl": true,
                    "fanProfile": "Freezy",
                    "minimumFanspeed": 0,
                    "offsetFanspeed": 5
                  },
                  "odmProfile": {
                    "name": "performance"
                  },
                  "odmPowerLimits": {
                    "tdpValues": []
                  },
                  "id": "0350erinz8o9lfg6puqi"
              }
              ]
            '';
  };
in
{
  imports = [
    # inputs.tuxedo-nixos.nixosModules.default
  ];
  # Tuxedo-rs is a rust-based GUI+CLI alternative to tuxedo-control-centre
  hardware.tuxedo-rs = {
    enable = true;
    tailor-gui.enable = true;
  };

  #powerManagement.cpuFreqGovernor = "performance"; #forced to schedutil by tuxedo control center
  # hardware.tuxedo-control-center.enable = true;
  systemd.services = {
    create-tcc-profile = {
      serviceConfig.Type = "oneshot";
      before = [ "tccd.service" ];
      wantedBy = [ "multi-user.target" ];
      script = ''
              mkdir -p /var/lib/tcc
              rm -f /var/lib/tcc/profiles
              ln -s ${tcc-profile} /var/lib/tcc/profiles
            '';
    };
  };
  systemd.services = {
    create-tailor-profile = {
      serviceConfig.Type = "oneshot";
      before = [ "tailord.service" ];
      wantedBy = [ "multi-user.target" ];
      script = ''
              mkdir -p /etc/tailord
              rm -f /etc/tailord/active_profile.json
              ln -s ${tailor-super-activate} /etc/tailord/active_profile.json
              mkdir -p /etc/tailord/profiles
              rm -f /etc/tailord/profiles/super.json
              ln -s ${tailor-super-profiles} /etc/tailord/profiles/super.json
              mkdir -p /etc/tailord/fan
              rm -f /etc/tailord/fan/super-fans.json
              ln -s ${tailor-super-fans} /etc/tailord/fan/super-fans.json
            '';
    };
  };
  # Redundant, done by enabling tuxedo-rs
  # hardware.tuxedo-keyboard.enable = true;
  # environment.systemPackages = [
  #   pkgs.linuxPackages.tuxedo-keyboard
  # ];
  # boot.kernelParams = [
  #   "tuxedo_keyboard.mode=0"
  #   "tuxedo_keyboard.brightness=10"
  #   "tuxedo_keyboard.color_left=0xff0a0a"
  # ];
  # Needed by tuxedo-nixos
  # Supposed to be set by tuxedo-nixos, but
  # not being seen for some reason
  # nixpkgs.config.permittedInsecurePackages = [
  #   "openssl-1.1.1u"
  #   "openssl-1.1.1t"
  #   "openssl-1.1.1w"
  #   "nodejs-14.21.3"
  #   "electron-13.6.9"
  # ];

}
