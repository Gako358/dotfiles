{
  pkgs,
  inputs,
  config,
  specialArgs,
  ...
}:
if specialArgs.hidpi
then {
  services.xserver = {
    enable = true;
    xkb = {
      variant = "";
      layout = "us";
    };
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gnome
      xdg-desktop-portal-wlr
    ];
  };
  environment.gnome.excludePackages =
    (with pkgs; [
      gnome-photos
      gnome-tour
    ])
    ++ (with pkgs.gnome; [
      cheese # webcam tool
      gnome-music
      gnome-contacts
      simple-scan
      epiphany # web browser
      geary # email reader
      evince # document viewer
      totem # video player
      tali # poker game
      iagno # go game
      hitori # sudoku game
      atomix # puzzle game
    ]);

  # ensure gnome-settings-daemon udev rules are enabled
  services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];
  # ensure telepathy is enable
  services.telepathy.enable = true;

  environment.systemPackages = with pkgs; [
    inputs.scramgit.defaultPackage.${pkgs.system}
    inputs.nvimFlake.defaultPackage.${pkgs.system}
  ];
}
else {}
