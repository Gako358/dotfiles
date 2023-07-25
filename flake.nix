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
    nixos-vscode-server.url = "github:msteen/nixos-vscode-server"; # VSCode server WSL
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
    hardware,
    nixos-wsl,
    flake-utils,
    home-manager,
    nixos-vscode-server,
    neovim-flake,
    scramgit,
    slock,
    nur,
    dwm,
    st,
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
      terangreal = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/terangreal
          nixosModules
          {
            # Set desktop environment
            desktop.xsession.enable = true;
            # Programs to enable
            programs.slack.enable = true;
            programs.teams.enable = true;
            programs.citrix.enable = false;
            programs.discord.enable = true;
            programs.intellij.enable = true;
            programs.moonlander.enable = true;
            programs.virt-manager.enable = true;
          }
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
          ./hosts/tuathaan
          nixosModules
          {
            # Set desktop environment
            desktop.xsession.enable = true;
            # Services enabled
            services.lock.enable = true;
            services.battery.enable = true;
            # Programs to enable
            programs.slack.enable = true;
            programs.teams.enable = true;
            programs.citrix.enable = true;
            programs.discord.enable = true;
            programs.intellij.enable = true;
            programs.virt-manager.enable = true;
          }
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
      angreal = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          ./hosts/angreal
          nixos-vscode-server.nixosModules.default
          nixosModules
          {
            # Set neccessary options
            services.vscode-server = {
              enable = true;
              installPath = "~/.vscode-server-insiders";
            };
          }
          {nixpkgs.overlays = builtins.attrValues overlays;}
          ({
            config,
            pkgs,
            ...
          }: {
            environment.systemPackages = [
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
          {
            # Development tools
            programs = {
              develop.vscode.enable = true;
              terminal = {
                # Gnome has default terminal
                kitty.enable = false;
                alacritty.enable = false;
                gnome-terminal.enable = false;
              };
            };
          }
        ];
      };
      "merrinx@tuathaan" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs;};
        modules = [
          ./home/tuathaan
          homeManagerModules
          {
            # Development tools
            programs = {
              develop.vscode.enable = true;
              terminal = {
                # Gnome has default terminal
                kitty.enable = false;
                alacritty.enable = false;
                gnome-terminal.enable = false;
              };
            };
          }
        ];
      };
      "nixos@angreal" = home-manager.lib.homeManagerConfiguration {
        pkgs = legacyPackages.x86_64-linux;
        extraSpecialArgs = {inherit inputs;};
        modules = [
          ./home/angreal
          homeManagerModules
          {
            # Development tools
            programs = {
              develop.vscode.enable = false;
              terminal = {
                # Gnome has default terminal
                kitty.enable = false;
                alacritty.enable = false;
                gnome-terminal.enable = false;
              };
            };
          }
        ];
      };
    };
  };
}
