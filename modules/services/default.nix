let
  more = {
    services = {
      pasystray.enable = true;
    };
  };
in
[
  ./gpg.nix
  ./hypridle.nix
  ./hyprpaper.nix
  ./mail.nix
  ./persist.nix
  ./secrets.nix
  more
]
