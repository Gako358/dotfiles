{ config
, pkgs
, ...
}: {
  home = {
    packages = with pkgs; [
      seahorse
    ];
    persistence."/persist/${config.home.homeDirectory}" = {
      directories = [
        ".config/sops/age"
        ".config/sops-nix"
        ".password-store"
      ];
    };
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

  programs = {
    gpg = {
      enable = true;
      homedir = "${config.home.homeDirectory}/.gnupg";
    };
    ssh.enable = true;
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
