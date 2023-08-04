{
  config,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.direnv;
in {
  options = {
    services.direnv = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable direnv integration for shells.";
      };
    };
  };
  config = mkIf (cfg.enable) {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };
}
