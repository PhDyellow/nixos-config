{config, pkgs, ...}:
{
  users.mutableUsers = false;
  users.users = {
    root.hashedPassword = "*";
  };
}
