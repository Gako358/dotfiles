{ lib
, pkgs
, inputs
, config
, ...
}:
{
  imports = [
    inputs.hyprland.nixosModules.default

    ./binds.nix
    ./rules.nix
    ./settings.nix
  ];

  config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
    environment = {
      systemPackages = [
        inputs.hyprland-contrib.packages.${pkgs.stdenv.hostPlatform.system}.grimblast
      ];
      pathsToLink = [ "/share/icons" ];
      variables.NIXOS_OZONE_WL = "1";
    };

    programs = {
      hyprland = {
        enable = true;
        withUWSM = true;
        package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
        portalPackage =
          inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
        plugins = with inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}; [
          # hyprbars
          # hyprexpo
        ];
      };
      # Patch fix for starting hyprland with start-hyprland
      uwsm.waylandCompositors.hyprland.binPath = lib.mkForce "/run/current-system/sw/bin/start-hyprland";
    };

    xdg.portal = {
      enable = true;
      xdgOpenUsePortal = true;
      config = {
        common.default = [ "gtk" ];
        hyprland.default = [
          "gtk"
          "hyprland"
        ];
      };
      extraPortals = [
        pkgs.xdg-desktop-portal-gtk
      ];
    };
  };
}
