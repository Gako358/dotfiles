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
      "email_keys/master-passwd" = { };
      "email_keys/work-passwd" = { };
      "email_keys/alias-private" = { };
      "email_keys/alias-service" = { };
      "email_keys/alias-social" = { };
      "private_keys/gako" = {
        mode = "0600";
        path = "${config.home.homeDirectory}/.ssh/id_gako";
      };
      "public_keys/gako" = {
        mode = "0600";
        path = "${config.home.homeDirectory}/.ssh/id_gako.pub";
      };
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
