{ self, inputs, ... }:
let
  inherit (inputs) nixpkgs home-manager;
  lib = nixpkgs.lib // home-manager.lib;

  mkNixosSystem = { hostName, profilePath }:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = { inherit inputs; };
      modules = [
        inputs.disko.nixosModules.disko
        inputs.home-manager.nixosModules.home-manager
        inputs.impermanence.nixosModules.impermanence
        inputs.sops-nix.nixosModules.sops

        "${self}/system"
        "${self}/hosts/${hostName}"

        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = { inherit inputs self; };
            backupFileExtension = ".hm-backup";
            users.merrinx = { ... }: {
              imports = [
                inputs.nix-colors.homeManagerModules.default
                inputs.impermanence.homeManagerModules.impermanence
                inputs.sops-nix.homeManagerModules.sops
                profilePath
              ];
            };
          };
        }
      ];
    };

  discoverHosts = path:
    let
      hostEntries = builtins.readDir path;
      hostDirs = lib.filterAttrs (_: type: type == "directory") hostEntries;
      hostNames = lib.attrNames hostDirs;
    in
    lib.listToAttrs (
      lib.map
        (hostName: {
          name = hostName;
          value = mkNixosSystem {
            inherit hostName;
            profilePath = "${self}/modules/profiles/${hostName}";
          };
        })
        hostNames
    );
in
{
  flake = {
    inherit lib;
    nixosConfigurations = discoverHosts ./.;
  };
}
