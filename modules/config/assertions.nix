_: {
  flake.nixosModules.config-assertions =
    {
      config,
      ...
    }:
    let
      cfgDesktop = config.environment.desktop;
      cfgGaming = config.environment.gaming;
      cfgServer = config.environment.server;
    in
    {
      assertions = [
        {
          assertion = !(cfgServer.enable && cfgDesktop.enable);
          message = ''
            Cannot enable both environment.server.enable and environment.desktop.enable simultaneously.
            Please choose one or disable the server environment if you need the desktop.
          '';
        }
        {
          assertion = !(cfgServer.enable && cfgGaming.enable);
          message = ''
            Cannot enable both environment.server.enable and environment.gaming.enable simultaneously.
            Please choose only one of these specialized environments or disable one.
          '';
        }
        {
          assertion = !(cfgGaming.enable && !cfgDesktop.enable);
          message = ''
            Cannot enable environment.gaming.enable without environment.desktop.enable being true.
            Gaming features require a desktop environment.
          '';
        }
      ];
    };
}
