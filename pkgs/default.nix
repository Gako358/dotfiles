# When you add custom packages, list them here
{pkgs ? import <nixpkgs> {}}: rec {
  pass-wofi = pkgs.callPackage ./pass-wofi {};
}
