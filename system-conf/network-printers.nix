{config, pkgs, ...}:
{
  services.printing.enable = true;
  # Discover printers on Wifi network
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
  services.printing.drivers = [
    pkgs.epson-escpr2
  ];
}
