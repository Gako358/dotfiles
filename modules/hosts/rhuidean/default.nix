{
  config,
  lib,
  inputs,
  ...
}:
{
  flake.nixosConfigurations.rhuidean = inputs.nixpkgs.lib.nixosSystem {
    specialArgs = {
      inherit inputs;
      self = config.flake;
    };
    modules = (lib.attrValues config.flake.nixosModules) ++ [
      inputs.disko.nixosModules.disko
      inputs.home-manager.nixosModules.home-manager
      inputs.impermanence.nixosModules.impermanence
      inputs.sops-nix.nixosModules.sops

      ./_machine.nix

      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = {
            inherit inputs;
            self = config.flake;
          };
          backupFileExtension = ".hm-backup";
          users.merrinx.imports = (lib.attrValues config.flake.homeModules) ++ [
            inputs.nix-colors.homeManagerModules.default
            inputs.sops-nix.homeManagerModules.sops
            ./_home.nix
          ];
        };
      }
    ];
  };
}
