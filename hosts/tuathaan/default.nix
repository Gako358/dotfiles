{ config, lib, ... }:

let
  office = "desc:HP Inc. HP E27u G4 CN41332M2N";
  laptop = "eDP-1";
in
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
      "${laptop},1920x1200,2560x1440,1"
      "${office},2560x1440,0x0,1"
      ",highrr,auto,1"
    ];

    workspace = [
      "1, monitor:${office}"
      "2, monitor:${office}"
      "3, monitor:${office}"
      "4, monitor:${office}"
      "5, monitor:${office}"
      "6, monitor:${office}"
      "7, monitor:${office}"
      "8, monitor:${office}"
      "9, monitor:${office}"
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
