let
  more = { config, lib, ... }: {
    options.desktop.environment = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable desktop environment";
      };
      windowManager = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum [ "hyprland" ]);
        default = "hyprland";
        description = "Set what window manager to use.";
      };
    };
  };
in
[
  ./hyprland
  more
]
