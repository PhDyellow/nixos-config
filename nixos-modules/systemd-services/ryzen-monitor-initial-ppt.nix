{config, pkgs, ...}:
{
  systemd.services = {
    ryzen-monitor-init = {
      serviceConfig.Type = "oneshot";
      after = [ "basic.target" ];
      wantedBy = [ "basic.target" ];
      script = with pkgs; ''
                  # 65W eco mode, "performance mode" set by Metabox
                  ${ryzen-monitor-ng}/bin/ryzen_monitor --set-ppt 88 --set-tdc 60 --set-edc 90
                '';
    };
  };
}
