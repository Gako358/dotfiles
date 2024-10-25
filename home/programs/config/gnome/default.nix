{
  pkgs,
  inputs,
  specialArgs,
  ...
}:
if specialArgs.hidpi
then {
  imports = [
    ./dconf.nix
  ];
  home.file."config.json" = {
    enable = true;
    source = ./floating-window-exception.json;
    target = ".config/pop-shell/config.json";
  };
}
else {}
