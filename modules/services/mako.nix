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
    iconPath = "${config.gtk.iconTheme.package}/share/icons/Papirus-Dark";
    font = fontType;
    padding = "10,20";
    anchor = "top-right";
    width = 460;
    height = 190;
    borderSize = "1";
    defaultTimeout = 7300; # 7 sec
    backgroundColor = "#${config.colorScheme.palette.base00}cc";
    borderColor = "#${config.colorScheme.palette.base0D}cc";
    textColor = "#${config.colorScheme.palette.base05}ff";
    layer = "overlay";
  };
}
