let
  more = {
    services = {
      flameshot.enable = true;
      pasystray.enable = true; # pulseaudio system tray
    };
  };
in [
  ./dunst.nix
  ./secret.nix
  more
]
