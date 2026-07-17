{config, pkgs, ...}:
{
  services.llama-cpp = {
    enable = true;
    extraFlags = [ ];
  };
}
