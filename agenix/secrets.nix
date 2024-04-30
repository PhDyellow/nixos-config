let 
  prime-ai = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKXuv00yB0lA43P4HGyTkund20jlbZvmV88uT3XcRRJM";
  prime-ai-phil = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDi6zlsK1OVU71zsgMRBzF48XKIj6zo24WCOYA+VrKir";
  nixos-vm-i = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGeqPX9gEb5yYi8jekBfXUhNe1z7tj19Dhc220ijpTBq";
  all-prime-ai = [prime-ai prime-ai-phil];
  all-systems = [prime-ai prime-ai-phil nixos-vm-i];
in
{
  "wpa_pwd.env.age".publicKeys = all-prime-ai;
  "user_phil_pwd.age".publicKeys = [prime-ai prime-ai-phil];
  "user_phil_pwd_vm.age".publicKeys = [nixos-vm-i prime-ai-phil];
  "cifs_dpbagje_share.age".publicKeys = all-systems;
  "prime_ai_tailscale.age".publicKeys = [prime-ai prime-ai-phil];
  "munge_key.age".publicKeys = all-prime-ai;
}
