_: {
  flake.nixosModules.hardware-ckb-next =
    {
      config,
      pkgs,
      lib,
      ...
    }:

    {
      config = lib.mkIf config.environment.gaming.enable {
        hardware.ckb-next = {
          enable = true;
          package = pkgs.ckb-next.overrideAttrs (old: rec {
            version = "0.6.2-unstable-2026-06-02";
            src = pkgs.fetchFromGitHub {
              owner = "ckb-next";
              repo = "ckb-next";
              rev = "5edd3c14810c1aa93be5adde471071b97ef108b4";
              hash = "sha256-h5jZqiK4hYqlLFPY9jmJEniarSHE4ZcoIX4Qv3QdELc=";
            };
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
    };
}
