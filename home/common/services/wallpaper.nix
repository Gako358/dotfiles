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
  cfg = config.services.wallpaper;
in {
  options = {
    services.wallpaper = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable the wallpaper service";
      };
    };
  };
  config = mkIf (cfg.enable && (config.desktop.environment == "dwm" || config.desktop.environment == "bspwm")) {
    programs.feh.enable = true;

    systemd.user.services.wallpaper = {
      Install.WantedBy = ["graphical-session.target"];
      Service.ExecStart = "${script}";
    };
  };
}
