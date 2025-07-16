{ config, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "rhuidean";
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

  programs.hyprland.settings = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
    monitor = [
      "virtual-1,2560x1440,0x0,1"
      "highrr,auto,1"
    ];

    workspace = [
      "1, monitor:virtual-1"
      "2, monitor:virtual-1"
      "3, monitor:virtual-1"
      "4, monitor:virtual-1"
      "5, monitor:virtual-1"
      "6, monitor:virtual-1"
      "7, monitor:virtual-1"
      "8, monitor:virtual-1"
      "9, monitor:virtual-1"
    ];
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
    mainDevice = "/dev/vda";
    extraStoreDisk.enable = false;
  };

  service.sops.enable = false;
}
