{lib, ...}:
with lib;
with builtins; {
  imports = [
    ./dwm.nix
    ./gnome.nix
    ./hyperland.nix
  ];
  options.desktop = {
    environment = mkOption {
      type = types.enum [
        "hyperland"
        "gnome"
        "none"
        "dwm"
      ];
      default = "none";
      description = "Desktop environment to use.";
    };
  };
}
