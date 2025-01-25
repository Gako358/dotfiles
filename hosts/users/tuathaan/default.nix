{ pkgs
, lib
, ...
}: {
  imports = [
    ./hardware-configuration.nix
  ];
  networking.hostName = "tuathaan";
  users.users = {
    merrinx = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      extraGroups = [ "wheel" "networkmanager" "docker" "video" "audio" "plugdev" ];
    };
  };

  services = {
    dbus.enable = true;
    xserver = {
      videoDrivers = [ "intel" ];
      displayManager = {
        sessionCommands = ''
          ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
        '';
      };
    };
  };
}
