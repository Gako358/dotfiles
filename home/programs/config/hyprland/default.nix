{
  pkgs,
  inputs,
  ...
}: {
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
    ./variables.nix
  ];
  # Enable hyprland
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    systemd.enable = true;

    settings = {
      cursor.inactive_timeout = 5;
    };
  };

  home.packages = [
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
