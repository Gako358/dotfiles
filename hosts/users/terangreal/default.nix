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
  desktop.environment = "gnome";
  users.users = {
    merrinx = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      extraGroups = ["wheel" "networkmanager" "docker" "libvirtd" "video" "audio"];
    };
  };

  services = {
    dbus.enable = true;
    xserver = {
      videoDrivers = ["amdgpu"];
      displayManager = {
        sessionCommands = ''
          ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
          ${pkgs.xorg.xrandr}/bin/xrandr --output DP-2 --mode 2560x1440 --pos 2560x0 --rotate normal --output HDMI-1 --primary --mode 2560x1440 --pos 0x0 --rotate normal
        '';
      };
    };
  };
}
