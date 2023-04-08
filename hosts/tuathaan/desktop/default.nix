{
  lib,
  ...
}:
with lib;
with builtins; {
  imports = [
    ./dwm.nix
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
      ];
      default = "dwm";
      description = "Desktop environment to use.";
    };
  };
}
