{lib, ...}:
with lib;
with builtins; {
  imports = [
    ./bspwm.nix
    ./network.nix
    ./picom.nix
    ./polybar.nix
    ./rofi-menu.nix
    ./rofi.nix
    ./sxhkd.nix
  ];
  options.desktop = {
    environment = mkOption {
      type = types.enum [
        "gnome"
        "bspwm"
        "none"
        "kde"
        "dwm"
      ];
      default = "none";
      description = "Desktop environment to use.";
    };
  };
}
