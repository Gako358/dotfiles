_: {
  flake.homeModules.services-network =
    {
      osConfig,
      lib,
      ...
    }:
    {
      services.network-manager-applet = lib.mkIf osConfig.environment.desktop.enable {
        enable = true;
      };
    };
}
