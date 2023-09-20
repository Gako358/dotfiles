{
  imports = [
    ./hyperland.nix
    ./hyprpaper.nix
  ];
  # Enable hyprland
  wayland.windowManager.hyprland = {
    enable = true;
    systemdIntegration = true;
    xwayland.enable = true;
  };
}
