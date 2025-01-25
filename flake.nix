{
  description = "MerrinX Flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Utilities for building our flake
    flake-utils.url = "github:numtide/flake-utils";

    # Extra flakes for modules, packages, etc
    hardware.url = "github:nixos/nixos-hardware"; # Convenience modules for hardware-specific quirks
    nur.url = "github:nix-community/NUR"; # User contributed pkgs and modules
    nix-colors.url = "github:misterio77/nix-colors"; # Color schemes for usage with home-manager
    impermanence.url = "github:riscadoa/impermanence"; # Utilities for opt-in persistance
    agenix.url = "github:ryantm/agenix"; # Secrets management

    # Hyprland
    hyprland.url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
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
    nvimFlake.url = "github:gako358/neovim";
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
          buildInputs = with pkgs; [
            nixpkgs-fmt
          ];
        });

      nixosConfigurations = {
        terangreal = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [
            ./hosts/configuration.nix
            ./hosts/users/terangreal
          ];
        };
        tuathaan = lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [
            ./hosts/configuration.nix
            ./hosts/users/tuathaan
          ];
        };
      };
      homeConfigurations = {
        "merrinx@terangreal" = lib.homeManagerConfiguration {
          pkgs = pkgsFor.x86_64-linux;
          extraSpecialArgs = {
            inherit inputs outputs nix-colors;
            hidpi = true;
          };
          modules = [
            ./home/home.nix
          ];
        };
        "merrinx@tuathaan" = lib.homeManagerConfiguration {
          pkgs = pkgsFor.x86_64-linux;
          extraSpecialArgs = {
            inherit inputs outputs nix-colors;
            hidpi = false;
          };
          modules = [
            ./home/home.nix
          ];
        };
      };
    };
}
