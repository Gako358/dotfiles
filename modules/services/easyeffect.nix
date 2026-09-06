_: {
  flake.homeModules.services-easyeffect =
    {
      osConfig,
      config,
      lib,
      ...
    }:
    let
      inherit (osConfig.environment) desktop;
    in
    {
      home.persistence."/persist" =
        lib.mkIf
          (
            !(
              osConfig.environment.desktop.windowManager == "kde"
              && lib.elem config.home.username osConfig.environment.desktop.kde.persistenceUsers
            )
          )
          {
            directories = [ ".config/easyeffects" ];
          };

      services.easyeffects = lib.mkIf desktop.enable {
        enable = true;
        preset = "noise-cancellation";
      };

      # `easyeffects --quit` stops over D-Bus, which hangs until TimeoutStopSec
      # during logout once the bus is gone; SIGTERM exits cleanly instead.
      systemd.user.services.easyeffects.Service = lib.mkIf desktop.enable {
        ExecStop = lib.mkForce "";
        TimeoutStopSec = lib.mkForce "5s";
      };

      xdg.configFile."easyeffects/input/noise-cancellation.json" = lib.mkIf desktop.enable {
        text = builtins.toJSON {
          input = {
            blocklist = [ ];
            plugins_order = [ "rnnoise#0" ];
            "rnnoise#0" = {
              bypass = false;
              enable-vad = true;
              input-gain = 0.0;
              model-name = "";
              output-gain = 0.0;
              release = 20.0;
              vad-thres = 91.0;
              wet = 0.0;
            };
          };
        };
      };
    };
}
