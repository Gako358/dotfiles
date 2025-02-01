{
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
}
