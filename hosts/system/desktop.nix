{
  pkgs,
  inputs,
  specialArgs,
  ...
}:
if specialArgs.hidpi
then {
  environment = {
    systemPackages = with pkgs; [
      morewaita-icon-theme
      qogir-icon-theme
      gnome-extension-manager

      inputs.scramgit.defaultPackage.${pkgs.system}
      inputs.nvimFlake.defaultPackage.${pkgs.system}
    ];

    gnome.excludePackages = with pkgs; [
      # gnome-text-editor
      atomix # puzzle game
      cheese # webcam tool
      epiphany # web browser
      evince # document viewer
      gedit
      geary # email reader
      gnome-characters
      gnome-connections
      gnome-console
      gnome-contacts
      gnome-font-viewer
      gnome-initial-setup
      gnome-maps
      gnome-music
      gnome-photos
      gnome-shell-extensions
      gnome-tour
      iagno # go game
      snapshot
      tali # poker game
      totem # video player
      hitori # sudoku game
      yelp # Help view
    ];
  };

  services.xserver = {
    enable = true;
    displayManager = {
      gdm.enable = true;
      autoLogin = {
        enable = true;
        user = "merrinx";
      };
    };
    desktopManager.gnome.enable = true;
  };

  systemd.services = {
    "getty@tty1".enable = false;
    "autovt@tty1".enable = false;
  };
}
else {
  services.greetd = let
    session = {
      command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --time-format '%I:%M %p | %a • %h | %F' --cmd Hyprland";
      user = "merrinx";
    };
  in {
    enable = true;
    restart = true;
    settings = {
      terminal.vt = 1;
      default_session = session;
    };
  };
}
