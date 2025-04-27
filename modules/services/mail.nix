{ lib
, pkgs
, config
, osConfig
, ...
}:
let
  certificatesFile = "${config.xdg.configHome}/protonmail/bridge-v3/cert.pem";
  cfg = config.service.mail;
in
{
  options.service.mail = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable email configuration.";
    };
    password = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Password for the email account.";
    };
  };

  config = lib.mkIf (cfg.enable && osConfig.environment.desktop.develop) {
    home = {
      packages = with pkgs; [
        protonmail-bridge
      ];
      persistence."/persist/${config.home.homeDirectory}" = {
        directories = [
          ".config/protonmail"
        ];
      };
    };

    sops.secrets = lib.mkIf osConfig.service.sops.enable {
      "email_user" = { };
      "email_home-passwd" = { };
      "email_work-passwd" = { };
      "email_alias-private" = { };
      "email_alias-service" = { };
      "email_alias-social" = { };
    };

    accounts.email = {
      maildirBasePath = "Mail";
      accounts = {
        personal = {
          primary = true;
          aliases = [
            "mugge.acrobat989@passinbox.com"
            "gako.footwork856@passinbox.com"
            "knut.sly692@passinbox.com"
          ];
          address = "merrinx@proton.me";
          userName = "merrinx@proton.me";
          passwordCommand = "${cfg.password}";
          realName = "merrinx";
          folders = {
            inbox = "INBOX";
            drafts = "Drafts";
            sent = "Sent";
            trash = "Trash";
          };
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            patterns = [ "*" "!All Mail" ];
            subFolders = "Verbatim";
          };
          mu.enable = true;
          imap = {
            host = "127.0.0.1";
            port = 1143;
            tls = {
              enable = true;
              useStartTls = true;
              # Use protonmail-bride -c and cert export
              inherit certificatesFile;
            };
          };
          smtp = {
            host = "127.0.0.1";
            port = 1025;
            tls = {
              enable = true;
              useStartTls = true;
              inherit certificatesFile;
            };
          };
          msmtp.enable = true;
        };
      };
    };

    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.mu.enable = true;
    services.mbsync = {
      enable = true;
      frequency = "*:0/1";
    };

    systemd.user.services.protonmail-bridge = {
      Unit = {
        Description = "Proton Mail Bridge";
        After = [ "network.target" ];
      };
      Service = {
        Restart = "always";
        RestartSec = 5;
        ExecStartPre = "${pkgs.coreutils}/bin/sleep 10";
        ExecStart = "${pkgs.protonmail-bridge}/bin/protonmail-bridge --no-window --noninteractive --log-level debug";
      };
      Install.WantedBy = [ "default.target" ];
    };

    # Wait for protonbridge service has started
    systemd.user.services.mbsync.Unit = {
      After = [ "protonmail-bridge.service" ];
      Requires = [ "protonmail-bridge.service" ];
      PartOf = [ "protonmail-bridge.service" ];
    };
  };
}
