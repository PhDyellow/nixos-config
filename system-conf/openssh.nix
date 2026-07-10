{config, pkgs, ...}:
{
  #Enable OpenSSH daemon
  #Primary use here is for agenix.
  #Enabling openssh creates host keys, which agenix uses for secrets
  #password entry is therefore disabled, and firewall ports are not opened
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    openFirewall = false;
  };

  programs.ssh = {
    #agentTimeout = "1h"; #request passphrase for keys every hour
    startAgent = true;
    askPassword = "systemd-ask-password";
    knownHosts = {
      github-ed25519 = {
        hostNames = [ "github.com" ];
        publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
      };
      github-rsa = {
        hostNames = [ "github.com" ];
        publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCj7ndNxQowgcQnjshcLrqPEiiphnt+VTTvDP6mHBL9j1aNUkY4Ue1gvwnGLVlOhGeYrnZaMgRK6+PKCUXaDbC7qtbW8gIkhL7aGCsOr/C56SJMy/BCZfxd1nWzAOxSDPgVsmerOBYfNqltV9/hWCqBywINIR+5dIg6JTJ72pcEpEjcYgXkE2YEFXV1JHnsKgbLWNlhScqb2UmyRkQyytRLtL+38TGxkxCflmO+5Z8CSSNY7GidjMIZ7Q4zMjA2n1nGrlTDkzwDCsw+wqFPGQA179cnfGWOWRVruj16z6XyvxvjJwbz0wQZ75XK5tKSb7FNyeIEs4TT4jk+S4dhPeAUC5y+bDYirYgM4GC7uEnztnZyaVWQ7B381AK4Qdrwt51ZqExKbQpTUNn+EjqoTwvqNj4kqx5QUCI0ThS/YkOxJCXmPUWZbhjpCg56i+2aB6CmK2JGhn57K5mj0MNdBXA4/WnwH6XoPWJzK5Nyu2zB3nAZp+S5hpQs+p1vN1/wsjk=";
      };
      github-ecdsa = {
        hostNames = [ "github.com" ];
        publicKey = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEmKSENjQEezOmxkZMy7opKgwFB9nkt5YRrYMjNuG5N87uRgg6CLrbo5wAdT/y6v0mKV0U2w0WZ2YB/++Tpockg=";
      };
      r-forge-ecdsa = {
        hostNames = [ "scm.r-forge.r-project.org" ];
        publicKey = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBBGIpa5hMQnQUuLeRRokZNQSETFDLDybxPMawXvSFoXrxJBfNMVQV5qw622q7/HjNoxi01p0UR4CqhoCT9Hkjow=";
      };
      r-forge-rsa = {
        hostNames = [ "scm.r-forge.r-project.org" ];
        publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEA6JrzVW2dIkO2nxz0ioJ/s+tIOLpr7C3fXGoeR9JXOQEvUnCR6KDCOZTzbM9rmYbT8CTZ/ZJOyrfC4Tdgsg2Uq7RYgFqiDT8fBIA/WYjhC2x06lUJNG9dhTu+pb6gFdOhELL7HNfXRzsQTCjs+H6GyNd6sdZxOYJGWjv4rshkVW6VyHqzHyI4hQI5dqTyShFijuWPH+b5oPd4xmu3OeMbseHN4Djvc/wfOJ3o6WGonrdjZKijhGNX8cNfCScMUmXQhmhMYcjbG/gP/X59pTmIADS5+lPTzj/QRBgvxHU19SFnEWu2TQkV46KR9In5F4lgcqbsKxntPPUdvygOGKEAYQ==";
      };
      dpbagje-ecdsa = {
        hostNames = [ "dpbagje.philjd.com" ];
        publicKey = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBKltuaDB9eLC3U0Z+mOySmWafB0KL1txZc3dDfEBTIGEoktgoH7oSaKW4MD1zdSbkysGUBRUTqITu/I8dBa4QiQ=";
      };
      dpbagje-ed25519 = {
        hostNames = [ "dpbagje.philjd.com" ];
        publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEPluafSiTm35xyEAPlhhZA2st4dPAY2JMVkPTw3CXYP";
      };
    };
  };
}
