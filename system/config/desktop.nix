{ lib
, ...
}: {
  options.environment.desktop = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable desktop environment";
    };
    windowManager = lib.mkOption {
      type = lib.types.nullOr (lib.types.enum [ "hyprland" "gnome" ]);
      default = "hyprland";
      description = "Set what window manager to use.";
    };
    develop = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Development toolchain";
    };
  };
}
