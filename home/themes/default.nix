let
  themes = {pkgs, ...}: {
    gtk = {
      enable = true;
      theme = {
        name = "Flat-Remix-GTK-White-Dark";
        package = pkgs.flat-remix-gtk;
      };
      iconTheme = {
        name = "Flat-Remix-Blue-Dark";
        package = pkgs.flat-remix-icon-theme;
      };
    };
  };
in [themes]
