_: {
  flake.homeModules.programs-raiderio =
    {
      osConfig,
      pkgs,
      lib,
      ...
    }:
    let
      raiderio-client = pkgs.callPackage ../../pkgs/raiderio-client { };
    in
    {
      config = lib.mkIf osConfig.environment.gaming.enable {
        home = {
          packages = [ raiderio-client ];
          persistence."/persist/" = {
            directories = [
              ".config/RaiderIO"
            ];
          };
        };
      };
    };
}
