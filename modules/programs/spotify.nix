{ osConfig
, config
, pkgs
, lib
, ...
}:
{
  config = lib.mkMerge [
    (lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
      sops.secrets = lib.mkIf osConfig.service.sops.enable {
        "spotify_id" = { };
        "spotify_secret" = { };
      };

      home.persistence."/persist/${config.home.homeDirectory}" = {
        directories = [
          ".cache/spotify-player"
        ];
      };

      programs.spotify-player = {
        enable = true;
        settings = {
          theme = "default";
          border_type = "Rounded";
          progress_bar_type = "Line";
          playback_window_position = "Top";
          play_icon = "";
          pause_icon = "";
          liked_icon = "";
          copy_command = {
            command = "wl-copy";
            args = [ ];
          };
          client_id = "65b708073fc0480ea92a077233ca87bd";
          client_port = 8888;
          ap_port = 443;
          device = {
            audio_cache = true;
            normalization = true;
            volume = 50;
          };
        };
      };
    })
    (lib.mkIf (osConfig.environment.desktop.windowManager == "gnome") {
      home.packages = [
        pkgs.spotify
      ];
    })
  ];
}
