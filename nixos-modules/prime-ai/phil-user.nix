{config, pkgs, ...}:
{
  age.secrets.user_phil_pwd.file = ./agenix/user_phil_pwd.age;
  users.users = {
    phil = {
      isNormalUser = true;
      extraGroups = ["wheel" "systemd-journal"];
      hashedPasswordFile = config.age.secrets.user_phil_pwd.path;
      uid = 1001;
      shell = pkgs.fish;
    };
  };
  programs.fish.enable = true;
}
