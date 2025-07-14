let
  colors = import ./colors.nix;
  themes =
    { config
    , pkgs
    , lib
    , ...
    }:
    {
      gtk = {
        enable = true;
        font = {
          name = "Inter";
          package = pkgs.google-fonts.override { fonts = [ "Inter" ]; };
          size = 9;
        };
        theme = {
          name = "Flat-Remix-GTK-White-Dark";
          package = pkgs.flat-remix-gtk;
        };
        iconTheme = {
          name = "Flat-Remix-Blue-Light";
          package = pkgs.flat-remix-icon-theme;
        };
        cursorTheme = {
          name = "capitaine-cursors-white";
          package = pkgs.capitaine-cursors;
          size = 16;
        };
        gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
      };
      xdg.configFile."gtk-4.0/gtk.css".enable = lib.mkForce false;
    };
in
[
  themes
  colors
]
