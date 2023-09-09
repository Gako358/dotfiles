let
  more = {
    services = {
      flameshot.enable = true;
    };
  };
in [
  ./polybar
  ./screenlocker
  ./background.nix
  ./dunst.nix
  ./picom.nix
  ./secret.nix
  more
]
