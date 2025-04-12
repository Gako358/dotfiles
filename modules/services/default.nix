let
  more = {
    services = {
      pasystray.enable = true;
    };
  };
in
[
  ./hypridle.nix
  ./hyprpaper.nix
  ./mail.nix
  ./secrets.nix
  more
]
