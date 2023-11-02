{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "tuathaan";
  # Set desktop environment and video drivers
  desktop.environment = "gnome";
  users.users = {
    merrinx = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      extraGroups = ["wheel" "networkmanager" "docker" "video" "audio" "plugdev"];
    };
  };

  services = {
    dbus.enable = true;
    xserver = {
      videoDrivers = ["intel" "displaylink"];
      displayManager = {
        # xrandr --setprovideroutputsource 2 0 This is after run xrandr --listproviders to identify the provider
        sessionCommands = ''
          ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
          ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-7 --primary --mode 5120x1440 --pos 1920x0 --rotate normal
        '';
      };
    };
  };
}
