{ config
, pkgs
, lib
, ...
}:

{
  hardware.ckb-next = lib.mkIf config.environment.gaming.enable {
    enable = true;
  };

  environment.systemPackages =
    with pkgs;
    (lib.mkIf config.environment.gaming.enable [
      libdbusmenu
      libsForQt5.libdbusmenu
    ]);
}
