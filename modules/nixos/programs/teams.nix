{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.teams;
in {
  options.programs.teams.enable = lib.mkEnableOption "Teams";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      teams
    ];
  };
}
