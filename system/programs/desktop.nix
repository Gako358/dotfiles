{ lib
, config
, pkgs
, ...
}: {
  environment.systemPackages = with pkgs; [
    glib
    gnome-calendar
    gnome-boxes
    gnome-weather
    gnome-system-monitor
  ];

  security = {
    pam = {
      services.greetd.enableGnomeKeyring = true;
      services.swaylock = { };
    };
    polkit.enable = true;
    rtkit.enable = true;
  };

  # Services needed for desktop
  services = {
    gvfs.enable = true;
    devmon.enable = true;
    udisks2.enable = true;
    upower.enable = true;
    accounts-daemon.enable = true;

    greetd =
      let
        session = {
          command = "${lib.getExe config.programs.hyprland.package}";
          user = "merrinx";
        };
      in
      {
        enable = true;
        restart = true;
        settings = {
          terminal.vt = 1;
          default_session = session;
        };
      };
    gnome = {
      evolution-data-server.enable = true;
      glib-networking.enable = true;
      gnome-keyring.enable = true;
      gnome-online-accounts.enable = true;
    };
  };
}
