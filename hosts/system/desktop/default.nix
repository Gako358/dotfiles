{lib, ...}:
with lib;
with builtins; {
  imports = [
    ./dwm.nix
    ./gnome.nix
  ];
  options.desktop = {
    environment = mkOption {
      type = types.enum [
        "gnome"
        "none"
        "dwm"
      ];
      default = "none";
      description = "Desktop environment to use.";
    };
  };
}
