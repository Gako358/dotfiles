{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./hyperland.nix
    ./hyprpaper.nix
    ./layout.nix
    ./monitors.nix
    ./systemd-fixes.nix
    ./tty.nix
  ];
  # Enable hyprland
  wayland.windowManager.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    systemdIntegration = true;
  };

  # Fake a tray, so apps can start
  systemd.user.targets.tray = {
    Unit = {
      Description = "Home Manager System Tray";
      Requires = ["graphical-session-pre.target"];
    };
  };
}
