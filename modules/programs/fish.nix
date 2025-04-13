{ pkgs, config, ... }:
let
  fenv = {
    inherit (pkgs.fishPlugins.foreign-env) src;
    name = "foreign-env";
  };
in
{
  home.persistence."/persist/${config.home.homeDirectory}" = {
    directories = [
      ".local/share/fish"
    ];
  };
  programs.fish = {
    enable = true;
    plugins = [ fenv ];
  };
}
