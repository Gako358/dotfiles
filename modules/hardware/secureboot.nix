{ inputs, ... }:
{
  flake.nixosModules.hardware-secureboot =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.hardware.secureboot;
    in
    {
      imports = [ inputs.lanzaboote.nixosModules.lanzaboote ];

      options.hardware.secureboot.enable = lib.mkEnableOption "UEFI Secure Boot via lanzaboote (signed UKIs)";

      config = lib.mkIf cfg.enable {
        # lanzaboote installs its own signed bootloader in place of systemd-boot.
        boot.loader.systemd-boot.enable = lib.mkForce false;

        boot.lanzaboote = {
          enable = true;
          pkiBundle = "/var/lib/sbctl";
        };

        environment.systemPackages = [ pkgs.sbctl ];

        # The PKI bundle holds the keys that sign every boot file; it lives on
        # the impermanent root, so it must be persisted or rebuilds can no
        # longer re-sign. The signed outputs land on the (non-wiped) ESP.
        environment.persistence."/persist".directories = [
          {
            directory = "/var/lib/sbctl";
            mode = "0700";
          }
        ];
      };
    };
}
