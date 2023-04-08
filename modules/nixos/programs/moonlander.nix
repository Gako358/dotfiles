{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.moonlander;
in {
  options.programs.moonlander.enable = lib.mkEnableOption "Moonlander keyboard configuration";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      wally-cli
    ];
  };
}
