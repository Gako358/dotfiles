_: {
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
      ];
    };
}
