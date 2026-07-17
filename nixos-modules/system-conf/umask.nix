{config, pkgs, ...}:
{
  environment.extraInit = "umask 0027";
}
