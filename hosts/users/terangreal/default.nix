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
  desktop.environment = "xmonad";
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
        '';
      };
    };
  };
}
