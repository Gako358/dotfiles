let
  themes = {pkgs, ...}: {
    gtk = {
      enable = true;
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
    };
  };
in [themes]
