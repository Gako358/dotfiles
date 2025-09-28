{ config
, pkgs
, lib
, ...
}:

{
  config = lib.mkIf config.environment.gaming.enable {
    hardware.ckb-next = {
      enable = true;
      package = pkgs.ckb-next.overrideAttrs (old: {
        cmakeFlags = (old.cmakeFlags or [ ]) ++ [ "-DUSE_DBUS_MENU=0" ];
      });
    };
    environment.persistence."/persist" = {
      users.merrinx = {
        directories = [
          ".config/ckb-next"
        ];
      };
    };
  };
}
