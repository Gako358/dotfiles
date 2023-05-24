{lib, ...}:
with lib;
with builtins; {
  imports = [
    ./dwm.nix
    ./wsl.nix
    ./gnome.nix
    ./xserver.nix
    ./environment.nix
  ];
  options.desktop = {
    environment = mkOption {
      type = types.enum [
        "gnome"
        "bspwm"
        "dwm"
        "kde"
        "wsl"
      ];
      default = "none";
      description = "Desktop environment to use.";
    };
  };
}
