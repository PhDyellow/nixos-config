{config, pkgs, ...}:
{
  programs.git ={
    enable = true;
    package = pkgs.gitFull;
    settings = {
      user.name = "Phil Dyer";
      user.email = "phildyer@protonmail.com";
      core = {
        autocrlf = "input";
      };
      github.user = "PhDyellow";
    };
  };
}
