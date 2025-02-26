{config, ...}: let
  palette = {
    background = "282c34";
    secondary_accent = "89b4fa";
    tertiary_accent = "f5f5f5";
  };
  fontType = "RobotoMono Nerd Font 12";
in {
  services.mako = {
    enable = true;
    iconPath = "${config.gtk.iconTheme.package}/share/icons/Papirus-Dark";
    font = fontType;
    padding = "10,20";
    anchor = "top-right";
    width = 460;
    height = 190;
    borderSize = 1;
    defaultTimeout = 7300; # 7 sec
    backgroundColor = "#${palette.background}aa"; # More transparent
    borderColor = "#${palette.secondary_accent}aa"; # More transparent
    textColor = "#${palette.tertiary_accent}aa"; # More transparent
    layer = "overlay";
  };
}
