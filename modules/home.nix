_: {
  flake.homeModules.base =
    _:
    let
      username = "merrinx";
      homeDirectory = "/home/${username}";
      configHome = "${homeDirectory}/.config";
    in
    {
      programs = {
        home-manager.enable = true;
        gh.enable = true;
      };

      xdg = {
        inherit configHome;
        enable = true;
      };

      home = {
        inherit username homeDirectory;
        stateVersion = "26.11";
      };

      systemd.user.startServices = "sd-switch";
      news.display = "silent";
    };
}
