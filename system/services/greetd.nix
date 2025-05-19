{ config
, lib
, ...
}: {
  config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
    security = {
      pam = {
        loginLimits = [
          { domain = "*"; type = "soft"; item = "nofile"; value = "65536"; }
          { domain = "*"; type = "hard"; item = "nofile"; value = "1048576"; }
        ];
        services.greetd.enableGnomeKeyring = true;
        services.swaylock = { };
      };
      polkit.enable = true;
      rtkit.enable = true;
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
    };
  };
}
