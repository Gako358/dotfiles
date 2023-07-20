{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.keyring;
in {
  options = {
    services.keyring = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable the keyring service.";
      };
    };
  };
  config = mkIf (cfg.enable && (config.desktop.environment == "dwm" || config.desktop.environment == "bspwm")) {
    home.packages = with pkgs; [
      gnome.seahorse
    ];

    programs = {
      gpg = {
        enable = true;
        homedir = "${config.home.homeDirectory}/.gnupg";
      };

      password-store = {
        enable = true;
        package = pkgs.pass.withExtensions (exts: [exts.pass-otp]);
        settings = {
          PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
        };
      };
    };

    services = {
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
        pinentryFlavor = "gnome3";
      };
    };
  };
}
