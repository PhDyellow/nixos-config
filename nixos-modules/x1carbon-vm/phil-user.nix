{config, pkgs, ...}:
{
  age.secrets.user_phil_pwd_vm.file = ./agenix/user_phil_pwd_vm.age;
  users.users = {
    phil = {
      isNormalUser = true;
      extraGroups = ["wheel"];
      hashedPasswordFile = config.age.secrets.user_phil_pwd_vm.path;
      uid = 1001;
      shell = pkgs.fish;
    };
  };
  programs.fish.enable = true;

}
