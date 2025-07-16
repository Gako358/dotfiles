{ config, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "terangreal";
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
      "DP-2,2560x1440,0x0,1"
      "DP-3,2560x1440,2560x0,1"
      ",highrr,auto,1"
    ];

    workspace = [
      "1, monitor:DP-3"
      "2, monitor:DP-2"
      "3, monitor:DP-2"
      "4, monitor:DP-2"
      "5, monitor:DP-2"
      "6, monitor:DP-3"
      "7, monitor:DP-3"
      "8, monitor:DP-3"
      "9, monitor:DP-3"
    ];
  };

  # Modules loaded
  program.qemu.enable = true;
}
