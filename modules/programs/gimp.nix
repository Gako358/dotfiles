_: {
  flake.homeModules.programs-gimp =
    {
      osConfig,
      pkgs,
      lib,
      ...
    }:
    {
      home.packages = lib.mkIf osConfig.environment.desktop.enable [
        pkgs.gimp
      ];
    };
}
