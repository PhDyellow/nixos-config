let 
  prime-ai = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKXuv00yB0lA43P4HGyTkund20jlbZvmV88uT3XcRRJM root@prime-ai-nixos";
  prime-ai-phil = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDi6zlsK1OVU71zsgMRBzF48XKIj6zo24WCOYA+VrKir phil_prime_ai@nixos";
  all_systems = [prime-ai prime-ai-phil];
in
{
  "wpa_pwd.env.age".publicKeys = all_systems;
  "user_phil_pwd.age".publicKeys = [prime-ai prime-ai-phil];
  "cifs_dpbagje_share.age".publicKeys = all_systems;
  "prime_ai_tailscale.age".publicKeys = [prime-ai prime-ai-phil];
  "github_pat.age".publicKeys = all_systems;
}
