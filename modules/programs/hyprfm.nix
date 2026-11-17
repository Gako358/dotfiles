_: {
  flake.nixosModules.programs-hyprfm =
    { config, lib, ... }:
    {
      config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
        services.usbmuxd.enable = true;
        systemd.tmpfiles.rules = [
          "L+ /usr/share/gvfs - - - - ${config.services.gvfs.package}/share/gvfs"
        ];
      };
    };

  flake.homeModules.programs-hyprfm =
    {
      osConfig,
      inputs,
      pkgs,
      lib,
      ...
    }:
    {
      home = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
        packages = [
          inputs.hyprfm.packages.${pkgs.stdenv.hostPlatform.system}.default
          pkgs.exiftool
          pkgs.ffmpeg
          pkgs.libimobiledevice
          pkgs.ntfs3g
          pkgs.rclone
          pkgs.usbmuxd
        ];
        persistence."/persist/" = {
          directories = [
            ".config/hyprfm"
          ];
        };
      };
    };
}
