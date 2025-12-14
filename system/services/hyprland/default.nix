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
    # ./tty.nix
  ];

  config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
    environment = {
      systemPackages = [
        inputs.hyprland-contrib.packages.${pkgs.stdenv.hostPlatform.system}.grimblast
      ];
      pathsToLink = [ "/share/icons" ];
      variables.NIXOS_OZONE_WL = "1";
    };

    programs.hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
      portalPackage = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
      plugins = with inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}; [
        # hyprbars
        # hyprexpo
      ];
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
