{pkgs, ...}: {
  services.xserver = {
    enable = true;
    libinput.enable = true;
    layout = "us";
    xkbVariant = "";
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };
  environment.gnome.excludePackages =
    (with pkgs; [
      gnome-photos
      gnome-tour
    ])
    ++ (with pkgs.gnome; [
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

  # Set fish as the default shell
  programs.fish.enable = true;
  # Set default shell to fish global
  # ensure gnome-settings-daemon udev rules are enabled
  services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];
  # ensure telepathy is enable
  services.telepathy.enable = true;
}
