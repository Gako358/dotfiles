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

  # Home modules to load
  program.hyprlock.defaultMonitor = "DP-2";

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
