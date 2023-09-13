let
  more = {
    services = {
      flameshot.enable = true;
    };
  };
in [
  ./background.nix
  ./dunst.nix
  ./picom.nix
  ./secret.nix
  more
]
