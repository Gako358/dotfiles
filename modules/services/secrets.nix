{ config
, lib
, ...
}:
let
  cfg = config.service.secrets;
in
{
  options.service.secrets = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Sops secrets";
    };
  };

  config = lib.mkIf cfg.enable {
    home.persistence."/persist/${config.home.homeDirectory}" = {
      directories = [
        ".config/sops/age"
        ".config/sops-nix"
      ];
    };

    sops = {
      age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
      defaultSopsFile = ../../secrets/default.yaml;
      validateSopsFiles = false;
      secrets = {
        "private_keys/gako" = {
          path = "${config.home.homeDirectory}/.ssh/id_rsa";
        };
        "public_keys/gako" = {
          path = "${config.home.homeDirectory}/.ssh/id_rsa.pub";
        };
      };
    };
  };
}
