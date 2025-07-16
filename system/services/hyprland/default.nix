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
    ./tty.nix
  ];

  config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
    environment = {
      systemPackages = [
        inputs.hyprland-contrib.packages.${pkgs.system}.grimblast
      ];
      pathsToLink = [ "/share/icons" ];
      variables.NIXOS_OZONE_WL = "1";
    };

    programs.hyprland = {
      enable = true;
      withUWSM = true;

      plugins = with inputs.hyprland-plugins.packages.${pkgs.system}; [
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
