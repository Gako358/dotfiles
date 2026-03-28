{ osConfig
, lib
, ...
}:
let
  inherit (osConfig.environment) desktop;
in
{
  services.easyeffects = lib.mkIf desktop.enable {
    enable = true;
    preset = "noise-cancellation";
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
}
