_: {
  flake.homeModules.programs-mugge =
    {
      osConfig,
      pkgs,
      inputs,
      lib,
      ...
    }:
    let
      muggePkgs = inputs.mugge.packages.${pkgs.stdenv.hostPlatform.system};
      muggeHosts = [
        "terangreal"
        "tuathaan"
      ];
    in
    {
      home.packages = lib.mkIf (lib.elem osConfig.networking.hostName muggeHosts) [
        muggePkgs.mugge
        muggePkgs.mugge-azure
      ];
    };
}
