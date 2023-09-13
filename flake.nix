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

    # My Neovim build
    neovim-flake.url = "github:gako358/neovim";
    # Scramgit
    scramgit.url = "github:gako358/scram";
    # Suckless Tools
    dwm.url = "github:gako358/dwm";
    st.url = "github:gako358/st";
    slock.url = "github:gako358/slock";

    # Fish
    fish-bobthefish-theme = {
      url = "github:gvolpe/theme-bobthefish";
      flake = false;
    };
    fish-keytool-completions = {
      url = "github:ckipp01/keytool-fish-completions";
      flake = false;
    };
  };

  outputs = {
    nixpkgs,
    hardware,
    flake-utils,
    home-manager,
    neovim-flake,
    fish-bobthefish-theme,
    fish-keytool-completions,
    scramgit,
    slock,
    dwm,
    st,
    nur,
    ...
  } @ inputs: let
    forAllSystems = nixpkgs.lib.genAttrs systems;
    systems = [
      "aarch64-linux"
      "i686-linux"
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ];
  in rec {
    fishOverlay = f: p: {
      inherit fish-bobthefish-theme fish-keytool-completions;
    };

    overlays = {
      default = import ./overlay {inherit inputs;};
      dwm = dwm.overlays.default;
      slock = slock.overlays.default;
      fish = fishOverlay;
    };
    templates = import ./templates;
    devShells = forAllSystems (system: {
      default = legacyPackages.${system}.callPackage ./shell.nix {};
    });

    legacyPackages = forAllSystems (system:
      import inputs.nixpkgs {
        inherit system;
        overlays = builtins.attrValues overlays;
        config.allowUnfree = true;
      });

    nixosConfigurations = {
      terangreal = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/configuration.nix
          ./hosts/users/terangreal
          {nixpkgs.overlays = builtins.attrValues overlays;}
          ({
            config,
            pkgs,
            ...
          }: {
            environment.systemPackages = [
              neovim-flake.defaultPackage.x86_64-linux
              scramgit.defaultPackage.x86_64-linux
              st.defaultPackage.x86_64-linux
            ];
          })
        ];
      };
      tuathaan = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/configuration.nix
          ./hosts/users/tuathaan
          {nixpkgs.overlays = builtins.attrValues overlays;}
          ({
            config,
            pkgs,
            ...
          }: {
            environment.systemPackages = [
              neovim-flake.defaultPackage.x86_64-linux
              scramgit.defaultPackage.x86_64-linux
              st.defaultPackage.x86_64-linux
            ];
          })
        ];
      };
    };
    homeConfigurations = {
      "merrinx@terangreal" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {
          inherit inputs;
          hidpi = true;
        };
        modules = [
          ./home/home.nix
          ./home/users/terangreal
        ];
      };
      "merrinx@tuathaan" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {
          inherit inputs;
          hidpi = false;
        };
        modules = [
          ./home/home.nix
          ./home/users/tuathaan
        ];
      };
    };
  };
}
