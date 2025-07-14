{ config
, pkgs
, lib
, ...
}:
{
  config = lib.mkMerge [
    (lib.mkIf config.environment.desktop.enable {
      services = {
        dbus = {
          enable = true;
          implementation = "broker";
          packages = [
            pkgs.gnome-keyring
            pkgs.gcr
          ];
        };
        gnome = {
          evolution-data-server.enable = true;
          glib-networking.enable = true;
          gnome-keyring.enable = true;
          gnome-online-accounts.enable = true;
        };
      };
      programs.dconf.enable = true; # Needed to manages user settings
      systemd = {
        user.services.polkit-gnome-authentication-agent-1 = {
          description = "polkit-gnome-authentication-agent-1";
          wantedBy = [ "graphical-session.target" ];
          wants = [ "graphical-session.target" ];
          after = [ "graphical-session.target" ];
          serviceConfig = {
            Type = "simple";
            ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
            Restart = "on-failure";
            RestartSec = 1;
            TimeoutStopSec = 10;
          };
        };
      };
    })
    (lib.mkIf (config.environment.desktop.windowManager == "gnome") {
      services = {
        xserver.enable = true;
        desktopManager.gnome.enable = true;
        displayManager.gdm.enable = true;
      };
      xdg.portal = {
        enable = true;
        extraPortals = with pkgs; [
          xdg-desktop-portal-wlr
          xdg-desktop-portal-gtk
          xdg-desktop-portal-gnome
        ];
      };
      environment.gnome.excludePackages = with pkgs; [
        gnome-photos
        gnome-tour
        cheese # webcam tool
        gnome-music
        gedit # text editor
        epiphany # web browser
        geary # email reader
        gnome-characters
        tali # poker game
        iagno # go game
        hitori # sudoku game
        atomix # puzzle game
        yelp # Help view
        gnome-contacts
        gnome-initial-setup
      ];

      environment.systemPackages = with pkgs; [
        gnome-tweaks
      ];
    })
  ];
}
