let
  more = {
    services = {
      pasystray.enable = true;
    };
  };
in
[
  ./dconf.nix
  ./gpg.nix
  ./hypridle.nix
  ./hyprpaper.nix
  ./mako.nix
  ./network.nix
  ./persist.nix
  more
]
