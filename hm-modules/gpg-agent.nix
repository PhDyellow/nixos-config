{config, pkgs, ...}:
{
  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 72000;
      pinentry.package = pkgs.pinentry-curses; # other interesting flavours emacs tty curses
      extraConfig = ''
                  allow-loopback-pinentry
                '';
      maxCacheTtl = 72000;
    };
  };
}
