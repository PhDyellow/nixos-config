{config, pkgs, ...}:
{
  nix = {
    package = pkgs.nixVersions.latest;
    settings = {
      system-features = [
        "recursive-nix"
        "kvm"
        "big-parallel"
        "nixos-test"
        "benchmark"
      ];
      experimental-features = [
        "nix-command"
        "flakes"
        "recursive-nix"
      ];
      substituters = [
        "https://nix-community.cachix.org" # for nix-community
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" #For Nix-community
      ];
    };
  };
}
