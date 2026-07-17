{config, pkgs, ...}:
{
  # if I need openconnect, see
  # https://search.nixos.org/options?channel=25.11&query=openconnect
  environment.systemPackages = with pkgs; [
  ];
  networking = {
    hostName = "prime-ai-nixos";
    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };
    wireless = {
      enable = true;
    };

    interfaces = {
      # eno1 = {
      #   useDHCP = false;
      # };
      # wlp0s20f3 = {
      #   useDHCP = false;
      # };
      enp4s0 = {
        useDHCP = true;
      };
      wlp5s0 = {
        useDHCP = true;
      };
    };
  };

}
