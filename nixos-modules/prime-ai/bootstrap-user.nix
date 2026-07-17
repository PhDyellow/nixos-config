{config, pkgs, ...}:
{
  users.users = {
    bootstrap = {
      isNormalUser = true;
      extraGroups = ["wheel"];
      password = "tmppwd";
    };
  };
}
