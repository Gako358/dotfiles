{
  osConfig,
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (osConfig.environment) desktop;
  cat = "${pkgs.coreutils}/bin/cat";
in
{
  program.quickshell.battery.enable = true;
  program.quickshell.lock.monitors = [
    "HP E27u"
    "DP-1"
    "HDMI-A-1"
    "eDP-1"
  ];

  services.hyprpaper.settings = lib.mkMerge [
    {
      wallpaper = [
        "eDP-1,${desktop.theme.wallpaper}"
        "desc:HP Inc. HP E27u G4 CN41332M2N,${desktop.theme.wallpaper}"
      ];
    }
  ];

  service = lib.mkMerge [
    {
      hypridle = {
        dpms = false;
        suspend = false;
      };
    }

    (lib.mkIf osConfig.service.sops.enable {
      mail.password = "${cat} ${config.sops.secrets."email_work-passwd".path}";
    })
  ];
}
