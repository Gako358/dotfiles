# home manager packages will install based on settings
# under enum, set desired desktop environment
{lib, ...}:
with lib;
with builtins; {
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
