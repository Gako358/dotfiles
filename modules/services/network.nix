{ config, lib, ... }:
{
  services.network-manager-applet = lib.mkIf config.desktop.environment.enable {
    enable = true;
  };
}
