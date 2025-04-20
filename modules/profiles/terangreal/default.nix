{ osConfig
, config
, pkgs
, lib
, ...
}:
let

  cat = "${pkgs.coreutils}/bin/cat";
in
{
  imports = [ ../../default.nix ];

  wayland.windowManager.hyprland.extraConfig = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") ''
    monitor=DP-2,2560x1440,0x0,1
    monitor=DP-3,2560x1440,2560x0,1
    monitor=,highrr,auto,1

    workspace = 1, monitor:DP-3
    workspace = 2, monitor:DP-2
    workspace = 3, monitor:DP-2
    workspace = 4, monitor:DP-2
    workspace = 5, monitor:DP-2
    workspace = 6, monitor:DP-3
    workspace = 7, monitor:DP-3
    workspace = 8, monitor:DP-3
    workspace = 9, monitor:DP-3
  '';

  # Home modules to load
  program.hyprlock.defaultMonitor = "DP-2";

  service = lib.mkMerge [
    {
      hypridle = {
        timeout = 3600;
        suspend = 600;
      };
    }

    (lib.mkIf osConfig.service.sops.enable {
      mail.password = "${cat} ${config.sops.secrets."email_home-passwd".path}";
    })
  ];
}
