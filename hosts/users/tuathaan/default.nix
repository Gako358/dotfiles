{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ./battery.nix
  ];

  networking.hostName = "tuathaan";
  # Set desktop environment and video drivers
  desktop.environment = "dwm";
  # Override dwm configuration
  environment.systemPackages = with pkgs; [
    (dwm.override {
      conf = ./patches/laptop.def.h;
    })
  ];
  # Application specific configuration
  work.citrix.enable = true;

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
        # TODO: ${pkgs.xorg.xrandr}/bin/xrandr --output DisplayPort-1 and the rest of arandr output
        # xrandr --setprovideroutputsource 2 0 This is after run xrandr --listproviders to identify the provider
        sessionCommands = ''
          ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
          ${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP1 --off --output DP1-1 --mode 2560x1440 --pos 4480x0 --rotate normal --output DP1-2 --off --output DP1-3 --mode 2560x1440 --pos 1920x0 --rotate normal --output DP2 --off --output DP3 --off --output DP4 --off --output HDMI1 --off --output VIRTUAL1 --off
        '';
      };
    };
  };
}
