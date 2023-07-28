{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.dbeaver;
in {
  options.programs.dbeaver.enable = lib.mkEnableOption "Dbeaver";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      dbeaver
    ];
  };
}
