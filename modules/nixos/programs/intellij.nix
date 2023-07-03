{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.intellij;
in {
  options.programs.intellij.enable = lib.mkEnableOption "intellij";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      jetbrains.idea-ultimate
    ];
  };
}
