_: {
  flake.homeModules.programs-chromium =
    {
      config,
      lib,
      ...
    }:
    {
      config = lib.mkIf config.programs.chromium.enable {
        home.persistence."/persist".directories = [ ".config/chromium" ];
      };
    };
}
