{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.slack;
in {
  options.programs.slack.enable = lib.mkEnableOption "Slack CLI";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      slack
    ];
  };
}
