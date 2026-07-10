{config, pkgs, ...}:
{
  networking = {
    hostName = "grass";
    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };
    interfaces = {
    };
  };
}
