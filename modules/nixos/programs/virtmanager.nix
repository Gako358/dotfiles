{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.programs.virt-manager;
in {
  options.programs.virt-manager = {
    enable = mkEnableOption "virt-manager";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [virt-manager];
  };
}
