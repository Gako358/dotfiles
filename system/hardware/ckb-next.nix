{ config
, pkgs
, lib
, ...
}:
{
  hardware.ckb-next = lib.mkIf config.environment.gaming.enable {
    enable = true;
    package = pkgs.ckb-next.overrideAttrs (old: {
      cmakeFlags = (old.cmakeFlags or [ ]) ++ [ "-DUSE_DBUS_MENU=0" ];
    });
  };
}
