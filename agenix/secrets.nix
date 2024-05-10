let 
  prime-ai = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKXuv00yB0lA43P4HGyTkund20jlbZvmV88uT3XcRRJM";
  prime-ai-phil = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDi6zlsK1OVU71zsgMRBzF48XKIj6zo24WCOYA+VrKir";
  nixos-vm-i = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAtI8IJwwmeUSBwsGEPdiTo1f16Gi7miMpdUl1zSVh8N";
  nixos-vm-i-phil = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBUBlCSiXpi3WHEPimV4jh3KYznNnJOOCwmZtinlCo1A";
  users-only = [nixos-vm-i-phil prime-ai-phil];
  all-prime-ai = [prime-ai prime-ai-phil];
  all-vm = [nixos-vm-i nixos-vm-i-phil];
  systems-only = [prime-ai nixos-vm-i];
  rekey-prime-ai = [prime-ai] ++ users-only;
  rekey-nixos-vm-i = [nixos-vm-i] ++ users-only;
  all-keys = users-only ++ systems-only;
in
{
  "wpa_pwd.env.age".publicKeys = rekey-prime-ai;
  "user_phil_pwd.age".publicKeys = rekey-prime-ai;
  "user_phil_pwd_vm.age".publicKeys = rekey-nixos-vm-i;
  "cifs_dpbagje_share.age".publicKeys = all-keys;
  "prime_ai_tailscale.age".publicKeys = rekey-prime-ai;
  "munge_key.age".publicKeys = rekey-prime-ai;
}
