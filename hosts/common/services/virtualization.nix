{
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.virtualisation;
in {
  options.virtualisation = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable virtualisation services";
    };
  };

  config = mkIf (cfg.enable && config.desktop.environment == "dwm") {
    virtualisation.docker = {
      enable = true;
      daemon.settings = {
        data-root = "/opt/docker";
      };
    };
    virtualisation.libvirtd.enable = true;
  };
}
