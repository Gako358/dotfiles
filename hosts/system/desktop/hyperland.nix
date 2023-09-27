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
  config = mkIf (cfg.environment == "hyperland") {
    # Set session variables
    environment.sessionVariables = {
      # If cursor is not visible, try to set this to "on".
      WLR_NO_HARDWARE_CURSORS = "1";
      # Electron apps use wayland
      NIXOS_OZONE_WL = "1";
    };

    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gnome
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };

    security = {
      polkit.enable = true;
    };

    environment.systemPackages = with pkgs; [
      pkgs.glib
      gnome.gnome-calendar
      gnome.gnome-boxes
      gnome.gnome-system-monitor
      gnome.gnome-weather

      # Neovim and Git build with system
      inputs.neovim-flake.defaultPackage.${pkgs.system}
      inputs.scramgit.defaultPackage.${pkgs.system}
    ];

    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        description = "polkit-gnome-authentication-agent-1";
        wantedBy = ["graphical-session.target"];
        wants = ["graphical-session.target"];
        after = ["graphical-session.target"];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };

    services = {
      gvfs.enable = true;
      devmon.enable = true;
      udisks2.enable = true;
      upower.enable = true;
      accounts-daemon.enable = true;
      gnome = {
        evolution-data-server.enable = true;
        glib-networking.enable = true;
        gnome-keyring.enable = true;
        gnome-online-accounts.enable = true;
      };
    };
  };
}
