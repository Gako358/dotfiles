{ config
, ...
}:
let
  cfgServer = config.environment.server;
  cfgDesktop = config.environment.desktop;
in
{
  imports = [
    ./desktop.nix
    ./server.nix
  ];

  assertions = [
    {
      assertion = !(cfgServer.enable && cfgDesktop.enable);
      message = ''
        Cannot enable both environment.server.enable and environment.desktop.enable simultaneously.
        Please choose one or disable the server environment if you need the desktop.
      '';
    }
  ];
}
