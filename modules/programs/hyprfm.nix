_: {
  flake.nixosModules.programs-hyprfm =
    { config, lib, ... }:
    {
      config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
        services.usbmuxd.enable = true;
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
      home.packages = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") [
        inputs.hyprfm.packages.${pkgs.stdenv.hostPlatform.system}.default
        pkgs.exiftool
        pkgs.ffmpeg
        pkgs.libimobiledevice
        pkgs.ntfs3g
        pkgs.rclone
        pkgs.usbmuxd
      ];
    };
}
