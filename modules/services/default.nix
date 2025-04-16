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
  ./persist.nix
  ./privacy.nix
  more
]
