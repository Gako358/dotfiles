{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "tuathaan";
  users.users = {
    merrinx = {
      isNormalUser = true;
      initialHashedPassword = "$6$S/5rsm5aa6ldGB90$uTqLQhPl6edMUy03a.bdzD2qILs28c74j1YcRHYHtFey/O1g7QDkpqRSpQBdA0hGLdimPMVrftjdyRn6dOWis.";
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      extraGroups = [ "wheel" "networkmanager" "docker" "video" "audio" "plugdev" ];
    };
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
