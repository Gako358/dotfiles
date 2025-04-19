{ config
, lib
, ...
}:
{
  home.persistence."/persist/${config.home.homeDirectory}" = lib.mkIf config.desktop.environment.enable {
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
}
