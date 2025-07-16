{ config, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "tuathaan";
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
      "eDP-1,1920x1200,2560x1440,1"
      "DP-8,2560x1440,0x0,1"
      "DP-6,2560x1440,2560x0,1"
      ",highrr,auto,1"
    ];

    workspace = [
      "1, monitor:DP-6"
      "2, monitor:DP-8"
      "3, monitor:DP-8"
      "4, monitor:DP-8"
      "5, monitor:DP-8"
      "6, monitor:DP-6"
      "7, monitor:DP-6"
      "8, monitor:DP-6"
      "9, monitor:DP-6"
    ];
  };
  # Modules loaded
  system = {
    disks.extraStoreDisk.enable = false;
    bluetooth.enable = true;
  };

  service = {
    blueman.enable = true;
    touchpad.enable = true;
  };
}
