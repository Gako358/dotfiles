{ self, inputs, ... }:
let
  nixpkgs = inputs.nixpkgs;
  home-manager = inputs.home-manager;
in
{
  flake = {
    lib = nixpkgs.lib // home-manager.lib;
    nixosConfigurations =
      let
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
                  extraSpecialArgs = { inherit inputs; };
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
      in
      {
        terangreal = mkNixosSystem {
          hostName = "terangreal";
          profilePath = "${self}/modules/profiles/terangreal";
        };
        tuathaan = mkNixosSystem {
          hostName = "tuathaan";
          profilePath = "${self}/modules/profiles/tuathaan";
        };
      };
  };
}
