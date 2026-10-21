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
  program.quickshell.lock.monitors = [ "DP-2" ];

  programs.merrinx-emacs.eca.nixMcp = {
    maxConcurrentJobs = 3;
    slice = {
      CPUWeight = 50;
      MemoryHigh = "16G";
      MemoryMax = "20G";
    };
  };

  services.hyprpaper.settings = lib.mkMerge [
    {
      wallpaper = [
        {
          monitor = "DP-2";
          path = desktop.theme.wallpaper;
        }
        {
          monitor = "DP-3";
          path = desktop.theme.wallpaper;
        }
      ];
    }
  ];

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
