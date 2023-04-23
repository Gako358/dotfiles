{
  lib,
  ...
}:
with lib;
with builtins; {
  imports = [
    ./kde.nix
    ./gnome.nix
    ./xserver.nix
  ];
  options.desktop = {
    environment = mkOption {
      type = types.enum [
        "gnome"
        "kde"
      ];
      default = "kde";
      description = "Desktop environment to use.";
    };
  };
}
