{ config
, lib
, ...
}:
with lib;
let
  cfg = config.service.secrets;
in
{
  options.service.secrets = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable secrets configuration.";
    };
  };

  config = mkIf cfg.enable {
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
        "email_user" = { };
        "email_home-passwd" = { };
        "email_work-passwd" = { };
        "email_alias-private" = { };
        "email_alias-service" = { };
        "email_alias-social" = { };
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
