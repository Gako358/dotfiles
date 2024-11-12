{
  specialArgs,
  pkgs,
  lib,
  ...
}:
if specialArgs.gaming
then {
  programs.steam = {
    enable = true;
    extest.enable = true;
    remotePlay.openFirewall = true;
  };
}
else {}
