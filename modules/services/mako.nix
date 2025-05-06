{ osConfig
, config
, lib
, ...
}:
let
  fontType = "RobotoMono Nerd Font 12";
in
{
  services.mako = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
    enable = true;
    settings = {
      icon-path = "${config.gtk.iconTheme.package}/share/icons/Papirus-Dark";
      font = fontType;
      padding = "10,20";
      anchor = "top-right";
      width = "460";
      height = "190";
      border-size = "1";
      default-timeout = "7300"; # 7 sec
      background-color = "#${config.colorScheme.palette.base00}cc";
      border-color = "#${config.colorScheme.palette.base0D}cc";
      text-color = "#${config.colorScheme.palette.base05}ff";
      layer = "overlay";
    };
  };
}
