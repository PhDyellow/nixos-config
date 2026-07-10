{config, pkgs, ...}:
{
  services.ollama = {
    enable = true;
    loadModels = [ ];
  };
}
