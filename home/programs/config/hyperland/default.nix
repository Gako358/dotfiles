{
  imports = [
    ./hyperland.nix
    ./hyprpaper.nix
    ./layout.nix
    ./monitors.nix
  ];
  # Enable hyprland
  wayland.windowManager.hyprland = {
    enable = true;
    systemdIntegration = true;
    xwayland.enable = true;
  };
}
