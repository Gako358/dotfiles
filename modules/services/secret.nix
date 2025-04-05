{ config
, inputs
, pkgs
, ...
}: {
  imports = [ inputs.sops-nix.homeManagerModules.sops ];
  home.packages = with pkgs; [
    seahorse
  ];

  sops = {
    age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
    defaultSopsFile = ../../secrets/default.yaml;
    validateSopsFiles = false;
    secrets = {
      email-private = { };
      email-user = { };
      email-passwd = { };
      email-alias-private = { };
      email-alias-service = { };
      email-alias-social = { };
    };
  };

  programs = {
    gpg = {
      enable = true;
      homedir = "${config.home.homeDirectory}/.gnupg";
    };

    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
      settings = {
        PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
      };
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };
  };
}
