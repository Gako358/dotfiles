{ config
, pkgs
, ...
}:
let
  cat = "${pkgs.coreutils}/bin/cat";
in
{
  sops.secrets = {
    "spotify_id" = { };
    "spotify_secret" = { };
  };

  home.persistence."/persist/${config.home.homeDirectory}" = {
    directories = [
      ".config/spotify-player"
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
      client_id = "${cat} ${config.sops.secrets."spotify_id".path}";
      client_port = 8888;
      ap_port = 443;
      device = {
        audio_cache = true;
        normalization = true;
        volume = 50;
      };
    };
  };
}
