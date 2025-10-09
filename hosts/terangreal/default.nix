{ config, lib, ... }:

let
  monitorLeft = "desc:Hewlett Packard HP Z27n CNK55013W1";
  monitorRight = "desc:HP Inc. HP Z27n G2 6CM9361M3C";
in
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
      "${monitorLeft},2560x1440,0x0,1"
      "${monitorRight},2560x1440,2560x0,1"
      ",highrr,auto,1"
    ];

    workspace = [
      "1, monitor:${monitorRight}"
      "2, monitor:${monitorLeft}"
      "3, monitor:${monitorLeft}"
      "4, monitor:${monitorLeft}"
      "5, monitor:${monitorLeft}"
      "6, monitor:${monitorRight}"
      "7, monitor:${monitorRight}"
      "8, monitor:${monitorRight}"
      "9, monitor:${monitorRight}"
    ];
  };

  # Modules loaded
  program.qemu.enable = true;
}
