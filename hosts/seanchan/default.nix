{ pkgs, config, ... }:
let
  cat = "${pkgs.coreutils}/bin/cat";
in
{
  imports = [
    ./hardware-configuration.nix
  ];
  networking.hostName = "seanchan";
  users.users = {
    merrinx = {
      isNormalUser = true;
      initialhasedpassword = "${cat} ${config.sops.secrets.user_password.path}";
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      extraGroups = [ "wheel" "networkmanager" "docker" "video" "audio" "plugdev" ];
    };
  };
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      General = {
        Name = "Computer";
        ControllerMode = "dual";
        FastConnectable = "true";
        Experimental = "true";
      };
      Policy = { AutoEnable = "true"; };
      LE = { EnableAdvMonInterleaveScan = "true"; };
    };
  };
}
