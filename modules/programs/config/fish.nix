{ pkgs, ... }:
let
  fenv = {
    inherit (pkgs.fishPlugins.foreign-env) src;
    name = "foreign-env";
  };
in
{
  programs.fish = {
    enable = true;
    plugins = [ fenv ];
  };
}
