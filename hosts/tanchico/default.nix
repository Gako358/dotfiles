{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "tanchico";
  users.users = {
    merrinx = {
      isNormalUser = true;
      initialHashedPassword = "$7$CU..../....re.hRJT/dAI5QtAnjQ2or/$nY1JgY1nvyNKZKGsFHnpCJUs9ABZEP5HqbQs04Vqmx4";
      openssh.authorizedKeys.keys = [
      ];
      extraGroups = [
        "wheel"
        "video"
        "audio"
        "plugdev"
      ];
    };
  };

  # Modules loaded
  environment = {
    desktop = {
      develop = false;
      windowManager = "gnome";
    };
    gaming.enable = true;
  };

  system.disks = {
    mainDevice = "/dev/sda";
    extraSteamDisk.enable = true;
    extraSteamDevice = "/dev/nvme0n1";
  };
}
