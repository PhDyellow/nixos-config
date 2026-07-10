{config, pkgs, ...}:
let
  gres = pkgs.writeTextFile {
    name = "gres.conf";
    text = ''
                Name=gpu Type=nvidia File=/dev/nvidia0
              '';
    destination = "/gres.conf";
  };
in
{
  environment.systemPackages = with pkgs; [
    apptainer
  ];
  age.secrets.munge_key = {
    file = ./agenix/munge_key.age;
    mode = "600";
    owner = "munge";
    group = "munge";
  };

  ## needed by slurm
  services.munge = {
    enable = true;
    password = config.age.secrets.munge_key.path;
  };
  services.slurm = {
    server = {
      enable = true;
    };
    client = {
      enable = true;
    };
    extraConfig = ''
                SlurmctldHost=${config.networking.hostName}

                SelectType=select/cons_tres
                SelectTypeParameters=CR_Core_Memory
                GresTypes=gpu
              '';
    extraConfigPaths = [
      "${gres}"
    ];
    # controlMachine = config.networking.hostName;
    # controlAddr="::1";
    nodeName = [
      "${config.networking.hostName} CPUs=11 CoresPerSocket=11 Sockets=1 RealMemory=62000  Gres=gpu:nvidia:1  State=UNKNOWN"
    ];
    partitionName = [
      "cpu Nodes=${config.networking.hostName} MaxCPUsPerNode=10 Default=YES MaxTime=UNLIMITED MaxMemPerNode=62000 State=UP"
      ## Reserve a cpu for the GPU jobs.
      "gpu Nodes=${config.networking.hostName} MaxCPUsPerNode=1  MaxTime=UNLIMITED MaxMemPerNode=62000 State=UP"
    ];
  };
}
