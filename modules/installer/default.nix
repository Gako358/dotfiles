{
  inputs,
  lib,
  config,
  ...
}:
let
  hostDirs = lib.filterAttrs (_: type: type == "directory") (builtins.readDir ../hosts);

  mkInstaller =
    host:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit inputs host;
        self = config.flake;
        flakeSrc = inputs.self;
      };
      modules = [
        "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
        inputs.disko.nixosModules.disko
        { nixpkgs.hostPlatform = "x86_64-linux"; }
        ./_image.nix
      ];
    };
in
{
  flake.nixosConfigurations = lib.mapAttrs' (
    host: _: lib.nameValuePair "installer-${host}" (mkInstaller host)
  ) hostDirs;
}
