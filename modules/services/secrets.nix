{ osConfig
, config
, lib
, ...
}:
{
  config = lib.mkIf osConfig.service.sops.enable {
    home.persistence."/persist/" = {
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
