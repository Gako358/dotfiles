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
    nixos-wsl.url = "github:nix-community/nixos-wsl"; # WSL utilities
    agenix.url = "github:ryantm/agenix"; # Secrets management

    # My Neovim build
    neovim-flake.url = "github:gako358/neovim";
    # Scramgit
    scramgit.url = "github:gako358/scram";
    # Suckless Tools
    dwm.url = "github:gako358/dwm";
    st.url = "github:gako358/st";
    slock.url = "github:gako358/slock";
  };

  outputs = {
    nixpkgs,
    home-manager,
    flake-utils,
    nur,
    neovim-flake,
    scramgit,
    dwm,
    st,
    slock,
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
    # Your custom packages and modifications
    overlays = {
      default = import ./overlay {inherit inputs;};
      dwm = dwm.overlays.default;
      slock = slock.overlays.default;
    };
    templates = import ./templates;
    nixosModules = import ./modules/nixos;
    homeManagerModules = import ./modules/home-manager;

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
      # DESKTOP
      terangreal = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/terangreal
          nixosModules
          {
            # Set desktop environment
            desktop.xsession.enable = true;
            # Services enabled
            # services.lock.enable = true;
            # Programs to enable
            programs.slack.enable = true;
            programs.citrix.enable = true;
            programs.vscode.enable = true;
            programs.discord.enable = true;
            programs.moonlander.enable = true;
            programs.virt-manager.enable = true;
          }
          {nixpkgs.overlays = builtins.attrValues overlays;}
          ({
              config,
              pkgs,
              ...
            } @ inputs: {
              environment.systemPackages = with pkgs; [
                neovim-flake.defaultPackage.x86_64-linux
                scramgit.defaultPackage.x86_64-linux
                st.defaultPackage.x86_64-linux
              ];
            })
        ];
      };
      # LAPTOP Work
      tuathaan = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/tuathaan
          nixosModules
          {
            # Set desktop environment
            desktop.xsession.enable = true;
            # Services enabled
            services.lock.enable = true;
            programs.slack.enable = true;
            programs.vscode.enable = true;
            programs.discord.enable = true;
            services.battery.enable = true;
            programs.virt-manager.enable = true;
          }
          {nixpkgs.overlays = builtins.attrValues overlays;}
          ({
              config,
              pkgs,
              ...
            } @ inputs: {
              environment.systemPackages = with pkgs; [
                neovim-flake.defaultPackage.x86_64-linux
                scramgit.defaultPackage.x86_64-linux
                st.defaultPackage.x86_64-linux
              ];
            })
        ];
      };
      # LAPTOP Kid
      sangreal = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/sangreal
          nixosModules
          {
            # Set desktop environment
            desktop.kde.enable = true;
            # Services enabled
            programs.vscode.enable = true;
            programs.discord.enable = true;
          }
        ];
      };
      # WSL
      angreal = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/angreal
          nixosModules
          {
            # Set neccessary options
          }
          {nixpkgs.overlays = builtins.attrValues overlays;}
          ({
              config,
              pkgs,
              ...
            } @ inputs: {
              environment.systemPackages = with pkgs; [
                neovim-flake.defaultPackage.x86_64-linux
              ];
            })
        ];
      };
    };
    homeConfigurations = {
      "merrinx@terangreal" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs;};
        modules = [
          ./home/terangreal
          homeManagerModules
        ];
      };
      "merrinx@tuathaan" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs;};
        modules = [
          ./home/tuathaan
          homeManagerModules
        ];
      };
      "mathoa@sangreal" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs;};
        modules = [
          ./home/sangreal
          homeManagerModules
        ];
      };
      "nixos@angreal" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs;};
        modules = [
          ./home/angreal
          homeManagerModules
        ];
      };
    };
  };
}
