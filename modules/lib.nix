{ inputs, ... }:
{
  # Re-export nixpkgs+home-manager lib for convenience (was `flake.lib` in old hosts/default.nix)
  flake.lib = inputs.nixpkgs.lib // inputs.home-manager.lib;
}
