let
  pkgs = import nixpkgs-unstable {system = "x86_64-linux";};
in
pkgs.mkShell {
  name = "secureboot_tools_shell";
  version = "1";
  buildInputs = with pkgs; [
    sbsigntool
    sbctl
    efitools
  ];
}
