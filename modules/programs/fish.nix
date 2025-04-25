{ pkgs, config, ... }:
let
  fenv = {
    inherit (pkgs.fishPlugins.foreign-env) src;
    name = "foreign-env";
  };
in
{
  home = {
    packages = with pkgs; [
      any-nix-shell # fish support for nix shell
      dive # explore docker layers
      duf # disk usage/free utility
      eza # a better `ls`
      fd # "find" for files
      jump # fast directory navigation
      ncdu # disk space info (a better du)
      nitch # minimal system information fetch
    ];
    persistence."/persist/${config.home.homeDirectory}" = {
      directories = [
        ".local/share/fish"
      ];
    };
  };
  programs.fish = {
    enable = true;
    plugins = [ fenv ];
  };
}
