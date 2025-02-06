{
  lib,
  config,
  inputs,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    glib
    gnome-calendar
    gnome-boxes
    gnome-weather
    gnome-system-monitor

    inputs.scramgit.defaultPackage.${pkgs.system}
    inputs.nvimFlake.defaultPackage.${pkgs.system}
  ];

  security = {
    # unlock GPG keyring on login
    pam.services.greetd.enableGnomeKeyring = true;
    polkit.enable = true;
  };

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

  # Services needed for desktop
  services = {
    gvfs.enable = true;
    devmon.enable = true;
    udisks2.enable = true;
    upower.enable = true;
    accounts-daemon.enable = true;

    greetd = let
      session = {
        command = "${lib.getExe config.programs.hyprland.package}";
        user = "merrinx";
      };
    in {
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
