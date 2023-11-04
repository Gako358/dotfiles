{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "terangreal";
  # Set desktop environment and video drivers
  desktop.environment = "hyprland";
  users.users = {
    merrinx = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      extraGroups = ["wheel" "networkmanager" "docker" "libvirtd" "video" "audio" "plugdev"];
    };
  };
  # ${pkgs.xorg.xrandr}/bin/xrandr --output DP-2 --mode 2560x1440 --pos 0x0 --rotate normal --output HDMI-A-1 --mode 2560x1440 --pos 2560x0 --rotate normal

  services = {
    dbus.enable = true;
    xserver = {
      videoDrivers = ["amdgpu"];
      displayManager = {
        sessionCommands = ''
          ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
        '';
      };
    };
  };
}
