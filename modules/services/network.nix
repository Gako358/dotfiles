{ config, lib, ... }:
{
  services.network-manager-applet = lib.mkIf config.environment.desktop.enable {
    enable = true;
  };
}
