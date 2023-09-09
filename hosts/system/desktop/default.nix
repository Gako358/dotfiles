{lib, ...}:
with lib;
with builtins; {
  imports = [
    ./dwm.nix
    ./gnome.nix
    ./xmonad.nix
  ];
  options.desktop = {
    environment = mkOption {
      type = types.enum [
        "xmonad"
        "gnome"
        "none"
        "dwm"
      ];
      default = "none";
      description = "Desktop environment to use.";
    };
  };
}
