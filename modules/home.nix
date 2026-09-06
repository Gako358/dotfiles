_: {
  flake.homeModules.base =
    {
      config,
      lib,
      ...
    }:
    {
      options.home.personalConfig.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable personal Home Manager configuration.";
      };

      config = {
        xdg = {
          configHome = "${config.home.homeDirectory}/.config";
          enable = true;
        };

        home.stateVersion = "26.11";
        systemd.user.startServices = "sd-switch";
        news.display = "silent";

        programs = {
          home-manager.enable = true;
          gh.enable = true;
        };
      };
    };
}
