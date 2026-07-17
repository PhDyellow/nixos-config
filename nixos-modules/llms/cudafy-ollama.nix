{config, pkgs, ...}:
{
  services.ollama = {
    acceleration = "cuda";
  };
}
