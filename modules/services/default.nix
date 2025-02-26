let
  more = {
    services = {
      pasystray.enable = true; # pulseaudio system tray
    };
  };
in [
  ./secret.nix
  more
]
