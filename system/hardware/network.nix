{ lib
, ...
}: {
  networking = {
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
    firewall.enable = false;
    extraHosts = ''
      127.0.0.1 tuathaan
      104.199.65.124 ap-gew4.spotify.com
    '';
  };

  users.users.merrinx.extraGroups = [ "networkmanager" ];
}
