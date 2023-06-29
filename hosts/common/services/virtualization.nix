{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.virtualisation;
in {
  options.services = {
    virtualisation = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable virtualisation services";
      };
    };
  };
  config = mkIf (cfg.enable && config.desktop.environment == "dwm") {
    environment.systemPackages = with pkgs; [
      docker-compose
    ];
    virtualisation.docker = {
      enable = true;
      daemon.settings = {
        data-root = "/opt/docker";
      };
    };
    virtualisation.podman.enable = true;
    virtualisation.libvirtd.enable = true;
  };
}
