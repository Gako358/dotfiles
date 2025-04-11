{ pkgs, ... }:
let
  cat = "${pkgs.coreutils}/bin/cat";
in
{
  imports = [
    ./disko.nix
    ./hardware-configuration.nix
  ];
  networking.hostName = "seanchan";
  users.users = {
    merrinx = {
      isNormalUser = true;
      # initialHashedPassword = "${cat} ${config.sops.secrets.user_password.path}";
      initialHashedPassword = "$6$q55Gre13G5rj57hp$F6fAXY09o.PcFv3TALyVB.B1JDntOC4ZSLDSKXfYyMpCDpV4MS.rL.gl3hcV6y6JmzOjzd16TQFNb.HPGP6z2.";
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      extraGroups = [ "wheel" "networkmanager" "docker" "video" "audio" "plugdev" ];
    };
  };
}
