{
  pkgs,
  lib,
  ...
}: let
  package = pkgs.pcmanfm;
in {
  environment.systemPackages = [package];
  services.dbus.packages = [package];
  services.gvfs.enable = lib.mkDefault true;
  services.udisks2.enable = lib.mkDefault true;
  services.devmon.enable = lib.mkDefault true;
}
