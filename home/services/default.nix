let
  more = {
    services = {
      pasystray.enable = true; # pulseaudio system tray
    };
  };
in [
  ./dunst.nix
  ./secret.nix
  more
]
