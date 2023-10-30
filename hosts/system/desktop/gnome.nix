{
  config,
  inputs,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "gnome") {
    services.xserver = {
      enable = true;
      libinput.enable = true;
      layout = "us";
      xkbVariant = "";
      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
      desktopManager.gnome.enable = true;
    };
    
    environment.systemPackages = with pkgs; [
      # Neovim and Git build with system
      inputs.neovim-flake.defaultPackage.${pkgs.system}
      inputs.scramgit.defaultPackage.${pkgs.system}
    ];
    
    # Set session variables
    environment.sessionVariables = {
      # If cursor is not visible, try to set this to "on".
      WLR_NO_HARDWARE_CURSORS = "1";
      # Electron apps use wayland
      NIXOS_OZONE_WL = "1";
    };
    # Electron apps use wayland
    xdg.portal.enable = true;
    
    environment.gnome.excludePackages =
      (with pkgs; [
        gnome-photos
        gnome-tour
      ])
      ++ (with pkgs.gnome; [
        gnome-music
        gnome-contacts
        simple-scan
        epiphany      # web browser
        geary         # email reader
        totem         # video player
        tali          # poker game
        iagno         # go game
        hitori        # sudoku game
        atomix        # puzzle game
      ]);

    # Set fish as the default shell
    programs.fish.enable = true;
    # ensure gnome-settings-daemon udev rules are enabled
    services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];
    # ensure telepathy is enable
    services.telepathy.enable = true;
  };
}
