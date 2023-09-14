let
  more = {
    services = {
      clipmenu.enable = true;
      flameshot.enable = true;
      pasystray.enable = true;
      udiskie = {
        enable = true;
        tray = "always";
      };
      volnoti.enable = true;
    };
  };
in [
  ./background.nix
  ./dunst.nix
  ./picom.nix
  ./secret.nix
  more
]
