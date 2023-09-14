let
  more = {
    services = {
      clipmenu.enable = true;
      flameshot.enable = true;
      pasystray.enable = true;
      volnoti.enable = true;
    };
  };
in [
  ./secret.nix
  more
]
