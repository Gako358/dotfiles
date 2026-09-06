_: {
  flake.homeModules.base =
    {
      config,
      ...
    }:
    {
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
