{pkgs, ...}: let
  microsoft-edge = pkgs.microsoft-edge-dev;
in {
  home.packages = [
    microsoft-edge
  ];
}
