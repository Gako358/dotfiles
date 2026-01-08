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
  program.hyprlock.defaultMonitor = "DP-2";
  services.hyprpaper.settings = {
    wallpaper = [
      "DP-2,${desktop.theme.wallpaper}"
      "DP-3,${desktop.theme.wallpaper}"
    ];
  };

  service = lib.mkMerge [
    {
      hypridle = {
        timeout = 10800;
        suspendTimer = 600;
      };
    }

    (lib.mkIf osConfig.service.sops.enable {
      mail.password = "${cat} ${config.sops.secrets."email_home-passwd".path}";
    })
  ];
}
