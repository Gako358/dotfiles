{ config
, pkgs
, lib
, ...
}:
let
  hyprlandConfig = pkgs.writeText "greetd-hyprland-config" ''
    exec-once = ${config.programs.regreet.package}/bin/regreet -L trace; hyprctl dispatch exit
    exec = systemctl --user import-environment
    debug:disable_logs = false
    misc {
        disable_hyprland_logo = true
        disable_splash_rendering = true
        disable_hyprland_qtutils_check = true
    }
  '';
in
{
  config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
    security = {
      pam = {
        loginLimits = [
          {
            domain = "*";
            type = "soft";
            item = "nofile";
            value = "65536";
          }
          {
            domain = "*";
            type = "hard";
            item = "nofile";
            value = "1048576";
          }
        ];
        services.greetd.enableGnomeKeyring = true;
        services.swaylock = { };
      };
      polkit.enable = true;
      rtkit.enable = true;
    };
    programs.regreet = {
      enable = true;
      theme = {
        package = pkgs.tokyonight-gtk-theme;
        name = "Tohyonight-Dark";
      };
    };
    services = {
      gvfs.enable = true;
      devmon.enable = true;
      udisks2.enable = true;
      upower.enable = true;
      accounts-daemon.enable = true;

      greetd =
        let
          session = {
            command = "${config.programs.hyprland.package}/bin/Hyprland --config ${hyprlandConfig}";
            # Uncomment for automatic login on boot
            # user = "merrinx";
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
    };
  };
}
