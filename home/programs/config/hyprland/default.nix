{
  pkgs,
  inputs,
  specialArgs,
  ...
}:
if !specialArgs.hidpi
then {
  imports = [
    ./binds.nix
    ./hypridle.nix
    ./hyprland.nix
    ./hyprlock.nix
    ./hyprpaper.nix
    ./monitors.nix
    ./rules.nix
    ./systemd-fixes.nix
    ./tty.nix
  ];
  # Enable hyprland
  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = true;

    settings = {
      cursor.inactive_timeout = 5;
    };
  };

  home.packages = [
    pkgs.ranger
    pkgs.pcmanfm
    pkgs.wayshot
    pkgs.sway-contrib.grimshot
    pkgs.pavucontrol
    pkgs.pulsemixer

    inputs.hyprland-contrib.packages.${pkgs.system}.grimblast
  ];

  # Fake a tray, so apps can start
  systemd.user.targets.tray = {
    Unit = {
      Description = "Home Manager System Tray";
      Requires = ["graphical-session-pre.target"];
    };
  };
}
else {}
