{pkgs, ...}: {
  home.packages = with pkgs; [
    eww-wayland   # Widgets
    jq            # JSON Processor
    socat         # Data Transfer
  ];

  home.file.".config/eww" = {
    source = ./eww;
    recursive = true;
  };
}
