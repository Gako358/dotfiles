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
  program.hyprlock.defaultMonitor = "DP-8";

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
