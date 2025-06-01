{ config
, pkgs
, lib
, ...
}: {
  xdg.portal = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
    enable = true;
    xdgOpenUsePortal = true;
    config = {
      common.default = [ "gtk" ];
      hyprland.default = [
        "gtk"
        "hyprland"
      ];
    };
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };
}
