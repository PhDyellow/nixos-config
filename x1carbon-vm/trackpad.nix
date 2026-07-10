{config, pkgs, ...}:
{
  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
  };
  # libinput is incompatible with synaptics, but is not disabled when
  # synaptics is enabled
  services.libinput.enable = false;
}
