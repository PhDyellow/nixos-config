{
  enable = true;
  enableDefaultConfig = false;
  matchBlocks = {
    "*" = {
      forwardAgent = false;
      addKeysToAgent = "no";
      compression = false;
      serverAliveInterval = 0;
      serverAliveCountMax = 3;
      hashKnownHosts = false;
      userKnownHostsFile = "~/.ssh/known_hosts";
      controlMaster = "no";
      controlPath = "~/.ssh/master-%r@%n:%p";
      controlPersist = "no";
    };
    rdm = {
      hostname = "data.qriscloud.org.au";
      user = "uqpdyer";
      forwardX11Trusted = true;
      identitiesOnly = true;
      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
    };
    getafix = {
      hostname = "getafix.smp.uq.edu.au";
      user = "uqpdyer";
      forwardX11Trusted = true;
      identitiesOnly = true;
      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
      port = 2022;
    };
    getafix0 = {
      hostname = "getafix1.smp.uq.edu.au";
      user = "uqpdyer";
      forwardX11Trusted = true;
      identitiesOnly = true;
      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
      port = 2022;
    };
    getafix1 = {
      hostname = "getafix2.smp.uq.edu.au";
      user = "uqpdyer";
      forwardX11Trusted = true;
      identitiesOnly = true;
      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
      port = 2022;
    };
    github = {
      hostname = "github.com";
      identitiesOnly = true;
      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
    };
    dogmatix = {
      hostname = "dogmatix.smp.uq.edu.au";
      user = "uqpdyer";
      forwardX11Trusted = true;
      identitiesOnly = true;
      identityFile = ["/home/phil/id_phil_prime_ai_nixos_ed25519"];
    };

  };
}
