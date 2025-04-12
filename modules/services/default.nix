let
  more = {
    services = {
      pasystray.enable = true; # pulseaudio system tray
    };
  };
in
[
  #./mail.nix
  #./secrets.nix
  more
]
