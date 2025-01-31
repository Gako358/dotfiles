{ lib
, config
, inputs
, pkgs
, ...
}: {
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    config = {
      common.default = [ "gtk" ];
      hyprland.default = [
        "gtk"
        "hyprland"
      ];
    };
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };

  security = {
    polkit.enable = true;
  };

  environment.systemPackages = with pkgs; [
    glib
    gnome-calendar
    gnome-boxes
    gnome-weather
    gnome-system-monitor

    inputs.scramgit.defaultPackage.${pkgs.system}
    inputs.nvimFlake.defaultPackage.${pkgs.system}
  ];

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

  services.greetd =
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
  # unlock GPG keyring on login
  security.pam.services.greetd.enableGnomeKeyring = true;
}
