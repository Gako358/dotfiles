{
  description = "MerrinX Flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Utilities for building our flake
    flake-utils.url = "github:numtide/flake-utils";

    # Secrets management
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Extra flakes for modules, packages, etc
    hardware.url = "github:nixos/nixos-hardware"; # Convenience modules for hardware-specific quirks
    nur.url = "github:nix-community/NUR"; # User contributed pkgs and modules
    nix-colors.url = "github:misterio77/nix-colors"; # Color schemes for usage with home-manager
    impermanence.url = "github:riscadoa/impermanence"; # Utilities for opt-in persistance

    # Hyprland
    hyprland.url = "github:hyprwm/hyprland";
    hyprland-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };

    # Scramgit
    scramgit.url = "github:gako358/scram";
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , nix-colors
    , ...
    } @ inputs:
    let
      inherit (self) outputs;
      lib = nixpkgs.lib // home-manager.lib;
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forEachSystem = f: lib.genAttrs systems (system: f pkgsFor.${system});
      pkgsFor = lib.genAttrs systems (system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        });
    in
    {
      inherit lib;
      overlays = {
        default = import ./overlay { inherit inputs outputs; };
      };
      templates = import ./templates;
      packages = forEachSystem (pkgs: import ./pkgs { inherit pkgs; });
      devShells = forEachSystem (pkgs:
        import ./shell.nix {
          inherit pkgs;
          buildInputs = [
          ];
        });

      nixosConfigurations = {
        terangreal = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./system
            ./hosts/terangreal
            {
              home-manager = {
                extraSpecialArgs = {
                  inherit inputs outputs;
                  hidpi = true;
                };
                backupFileExtension = ".hm-backup";
                sharedModules = [
                  inputs.sops-nix.homeManagerModules.sops
                ];
                users.merrinx = { ... }: {
                  nixpkgs.config.allowUnfree = true;
                  imports = [ ./modules ];
                };
              };
            }
          ];
        };
        tuathaan = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./system
            ./hosts/tuathaan
            {
              home-manager = {
                # useGlobalPkgs = true; # TODO: Fix this on new install
                # useUserPackages = true;
                extraSpecialArgs = {
                  inherit inputs outputs;
                  hidpi = false;
                };
                backupFileExtension = ".hm-backup";
                users.merrinx = { ... }: {
                  nixpkgs.config.allowUnfree = true;
                  imports = [ ./modules ];
                };
              };
            }
          ];
        };
      };
    };
}
