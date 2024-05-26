{pkgs, ...}: {
  home.packages = with pkgs; [
    eww # Widgets
    jq # JSON Processor
    socat # Data Transfer
  ];

  home.file.".config/eww" = {
    source = ./eww;
    recursive = true;
  };
}
