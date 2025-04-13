{ pkgs, ... }:
let
  fenv = {
    inherit (pkgs.fishPlugins.foreign-env) src;
    name = "foreign-env";
  };
in
{
  home.persistence."/persist/home/merrinx" = {
    directories = [
      ".local/share/fish"
    ];
  };
  programs.fish = {
    enable = true;
    plugins = [ fenv ];
  };
}
