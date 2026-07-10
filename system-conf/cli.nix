{config, pkgs, ...}:
{
  environment.systemPackages = with pkgs; [
    ripgrep
    nil # nix language server
    openssl
    vim
    git
    gitSVN
    wget
    curl
    dig
    pigz
    certbot
    squashfsTools
    squashfs-tools-ng
    inputs.ragenix.packages.x86_64-linux.default
  ];
}
