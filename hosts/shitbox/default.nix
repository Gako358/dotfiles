{ config, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "shitbox";
  users.users = {
    leif = {
      isNormalUser = true;
      initialHashedPassword = "$7$CU..../....7emauu/nSIai9Z3k.5nme1$6FDaMoeVeQBls.bZ3FsswOVWoeB.ILPtcIAqZh24f54";
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
      "DP-7,2560x1440,0x0,1"
      "DP-8,2560x1440,2560x0,1"
      ",highrr,auto,1"
    ];

    workspace = [
      "1, monitor:DP-7"
      "2, monitor:DP-8"
      "3, monitor:DP-8"
      "4, monitor:DP-8"
      "5, monitor:DP-8"
      "6, monitor:DP-7"
      "7, monitor:DP-8"
      "8, monitor:DP-8"
      "9, monitor:DP-7"
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
