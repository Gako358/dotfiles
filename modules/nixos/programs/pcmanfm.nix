{ pkgs
, config
, lib
, ...
}:
with lib;
with builtins; let
  cfg = config.programs.pcmanfm;
  package = pkgs.pcmanfm;
in
{
  options.programs.pcmanfm = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Installed by default, if xsession is enabled
      '';
    };
  };
  config = mkIf (cfg.enable && config.desktop.xsession.enable) {
    environment.systemPackages = [ package ];
    services.dbus.packages = [ package ];
    services.gvfs.enable = lib.mkDefault true;
    services.udisks2.enable = lib.mkDefault true;
    services.devmon.enable = lib.mkDefault true;
  };
}
