{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  script = pkgs.writeShellScript "wallpaper" ''
    export PATH=${lib.makeBinPath [
      pkgs.feh
    ]}
    feh --bg-fill /home/merrinx/Sources/archive/images/wallpapers/dark_mountains.jpg
  '';
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "dwm" || cfg.environment == "bspwm") {
    programs.feh.enable = true;

    systemd.user.services.wallpaper = {
      Install.WantedBy = ["graphical-session.target"];
      Service.ExecStart = "${script}";
    };
  };
}
