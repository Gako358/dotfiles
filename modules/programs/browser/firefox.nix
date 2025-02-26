{
  pkgs,
  specialArgs,
  ...
}: let
  inherit (specialArgs) hidpi;
  mailClient = pkgs.thunderbird;

  dpiSettings = {
    "layout.css.devPixelsPerPx" =
      if hidpi
      then "0.9"
      else "-1.0";
  };

  settings =
    {
      "browser.link.open_newwindow" = true;

      # disable all the annoying quick actions
      "browser.urlbar.quickactions.enabled" = false;
      "browser.urlbar.quickactions.showPrefs" = false;
      "browser.urlbar.shortcuts.quickactions" = false;
      "browser.urlbar.suggest.quickactions" = false;
    }
    // dpiSettings;
in {
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    profiles = {
      default = {
        id = 0;
        inherit settings;
      };
    };
  };
  home.packages = [mailClient];
}
