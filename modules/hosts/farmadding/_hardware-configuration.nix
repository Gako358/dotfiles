{ lib, modulesPath, ... }:

let
  installation = import ./_installation.nix;
in
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  fileSystems = lib.mkIf (!installation.ready) {
    "/" = {
      device = "none";
      fsType = "tmpfs";
    };
    "/persist" = {
      device = "none";
      fsType = "tmpfs";
    };
  };
}
