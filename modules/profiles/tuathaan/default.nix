{ osConfig
, config
, pkgs
, lib
, ...
}:
let

  inherit (osConfig.environment) desktop;
  cat = "${pkgs.coreutils}/bin/cat";
in
{
  imports = [ ../../default.nix ];

  # Home modules to load
  program.hyprlock.defaultMonitor = "desc:HP Inc. HP E27u G4 CN41332M2N";
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
