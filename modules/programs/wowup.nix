_: {
  flake.homeModules.programs-wowup =
    {
      osConfig,
      pkgs,
      lib,
      ...
    }:
    {
      config = lib.mkIf osConfig.environment.gaming.enable {
        home = {
          packages = with pkgs; [
            wowup-cf
          ];
          persistence."/persist/" = {
            directories = [
              ".config/WowUpCf"
            ];
          };
        };
      };
    };
}
