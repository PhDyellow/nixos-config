{config, lib, pkgs, ...}:
{
  imports = [
    inputs.ragenix.nixosModules.age
  ];
  # Under impermanence, /persistent/etc/ssh is not
  # mounted to /etc/ssh when age attempts to decrypt
  # secrets
  age.identityPaths = [
    "/persistent/etc/ssh/ssh_host_ed25519_key"
    "/persistent/etc/ssh/ssh_host_rsa_key"
  ];
}
