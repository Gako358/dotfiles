let
  more = {
    services = {
      pasystray.enable = true; # pulseaudio system tray
    };
  };
in
[
  ./mail.nix
  ./secret.nix
  more
]
