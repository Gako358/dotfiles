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
  environment.systemPackages = with pkgs; [
    xorg.xbacklight
  ];

  users.users = {
    merrinx = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      extraGroups = ["wheel" "networkmanager" "docker" "video" "audio"];
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
          ${pkgs.xorg.xrandr}/bin/xrandr --output DP-5 --mode 2560x1440 --pos 4480x0 --rotate normal --output DP-7 --mode 2560x1440 --pos 1920x0 --rotate normal --output eDP-1 --primary --mode 1920x1080 --pos 0x360 --rotate normal
        '';
      };
    };
  };
}
