{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.sys.audio;
in {
  config = mkIf (cfg.server == "pulse") {
    sound.enable = true;

    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.support32Bit = true;
    hardware.pulseaudio.package = pkgs.pulseaudioFull;

    services.mpd = {
      enable = true;
      musicDirectory = "/home/merrinx/Music";
      extraConfig = ''
        audio_output {
          type "pulse"
          name "PulseAudio"
        }
      '';
    };
    # Install needed packages
    environment.systemPackages = with pkgs; [
      mpc-cli
    ];
  };
}
