{
  description = "MerrinX Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    disko = {
      url = "github:nix-community/disko/latest";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    hardware.url = "github:nixos/nixos-hardware";
    hyprland.url = "github:hyprwm/hyprland";
    hyprland-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "hyprland/nixpkgs";
    };
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence.url = "github:nix-community/impermanence";
    nix-colors.url = "github:misterio77/nix-colors";
    nur.url = "github:nix-community/NUR";
    scramgit.url = "github:gako358/scram";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zen-browser = {
      url = "github:youwen5/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
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
            master = true;
          };
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./system
            ./hosts/terangreal
            {
              home-manager = {
                extraSpecialArgs = {
                  inherit inputs outputs;
                  master = true;
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
        tuathaan = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
            master = false;
          };
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./system
            ./hosts/tuathaan
            {
              home-manager = {
                # useGlobalPkgs = true; # TODO: Fix this on new install
                # Packages are then in ~/.local/state/home-manager/gcroots/current-home
                # Or /etc/profiles if enabled useUserPackages
                # useUserPackages = true;
                extraSpecialArgs = {
                  inherit inputs outputs;
                  master = false;
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
        # Profile for testing in KVM
        seanchan = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
            master = false;
          };
          modules = [
            inputs.disko.nixosModules.disko
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
            ./system
            ./hosts/seanchan
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = {
                  inherit inputs outputs;
                  master = false;
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
