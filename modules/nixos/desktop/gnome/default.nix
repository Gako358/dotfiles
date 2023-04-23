{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop.gnome;
in {
  options.desktop.gnome.enable = lib.mkEnableOption "Gnome Desktop";
  config = lib.mkIf cfg.enable {
    environment.gnome.excludePackages =
      (with pkgs; [
        gnome-photos
        gnome-tour
      ])
      ++ (with pkgs.gnome; [
        cheese # webcam tool
        gnome-music
        gnome-weather
        gnome-system-monitor
        gnome-contacts
        simple-scan
        epiphany # web browser
        geary # email reader
        evince # document viewer
        totem # video player
        tali # poker game
        iagno # go game
        hitori # sudoku game
        atomix # puzzle game
      ]);

    # Systray Icons
    environment.systemPackages = with pkgs; [
      gnomeExtensions.appindicator
      gnomeExtensions.vitals
      gnomeExtensions.tiling-assistant
      gnomeExtensions.user-themes
      gnomeExtensions.sound-output-device-chooser
      gnomeExtensions.caffeine
      gnomeExtensions.dash-to-dock
      gnome.gnome-tweaks
      gnome.dconf-editor
      gnome.gnome-notes
      spotify
      arandr
      evince
    ];
    # ensure gnome-settings-daemon udev rules are enabled
    services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];
    # ensure telepathy is enable
    services.telepathy.enable = true;
  };
}
