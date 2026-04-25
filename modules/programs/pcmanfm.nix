_: {
  flake.homeModules.programs-pcmanfm =
    {
      osConfig,
      pkgs,
      lib,
      ...
    }:
    {
      home.packages = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") [
        pkgs.pcmanfm
      ];
    };
}
