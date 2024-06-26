{
  specialArgs,
  pkgs,
  lib,
  ...
}:
if !specialArgs.hidpi
then {
  programs.steam = {
    enable = true;
    extest.enable = true;
    remotePlay.openFirewall = true;
  };
}
else {}
