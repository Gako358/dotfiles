{pkgs, ...}: let
  microsoft-edge = pkgs.microsoft-edge;
in {
  home.packages = [
    microsoft-edge
  ];
}
