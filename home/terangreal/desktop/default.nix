# home manager packages will install based on settings
# under enum, set desired desktop environment
{ config
, lib
, pkgs
, ...
}:
with lib;
with builtins; {
  options.desktop = {
    environment = mkOption {
      type = types.enum [
        "gnome"
        "bspwm"
        "dwm"
      ];
      default = "gnome";
      description = "Desktop environment to use.";
    };
  };
}
