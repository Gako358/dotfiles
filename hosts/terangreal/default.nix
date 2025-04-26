{
  imports = [
    ./hardware-configuration.nix
  ];

  networking.hostName = "terangreal";
  users.users = {
    merrinx = {
      isNormalUser = true;
      initialHashedPassword = "$6$S/5rsm5aa6ldGB90$uTqLQhPl6edMUy03a.bdzD2qILs28c74j1YcRHYHtFey/O1g7QDkpqRSpQBdA0hGLdimPMVrftjdyRn6dOWis.";
      openssh.authorizedKeys.keys = [
      ];
      extraGroups = [ "wheel" "video" "audio" "plugdev" ];
    };
  };

  # Modules loaded
  program.qemu.enable = true;
}
