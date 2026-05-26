_: {
  flake.homeModules.programs-onefetch =
    {
      osConfig,
      pkgs,
      lib,
      ...
    }:
    {
      config = lib.mkIf osConfig.environment.desktop.enable {
        home.packages = [
          pkgs.onefetch
        ];
      };
    };
}
