_: {
  flake.homeModules.services-mail =
    {
      lib,
      pkgs,
      config,
      osConfig,
      ...
    }:
    let
      certificatesFile = "${config.xdg.configHome}/protonmail/bridge-v3/cert.pem";
      cfg = config.service.mail;

      davmailDir = "${config.xdg.configHome}/davmail";
      davmailProperties = "${davmailDir}/davmail.properties";

      davmailConfigBase = pkgs.writeText "davmail.properties" ''
        davmail.server=true
        davmail.mode=O365Manual
        davmail.url=https://outlook.office365.com/EWS/Exchange.asmx
        davmail.oauth.persistToken=true
        davmail.oauth.clientId=${cfg.work.oauthClientId}
        davmail.oauth.tenantId=${cfg.work.tenantId}
        davmail.oauth.redirectUri=${cfg.work.redirectUri}
        davmail.bindAddress=127.0.0.1
        davmail.allowRemote=false
        davmail.imapPort=${toString cfg.work.imapPort}
        davmail.smtpPort=${toString cfg.work.smtpPort}
        davmail.caldavPort=1080
        davmail.ldapPort=1389
        davmail.popPort=0
        davmail.ssl.nosecurecaldav=false
        davmail.ssl.nosecureimap=false
        davmail.ssl.nosecureldap=false
        davmail.ssl.nosecuresmtp=false
        davmail.imapAutoExpunge=true
        davmail.smtpSaveInSent=true
        davmail.enableKeepAlive=true
        log4j.rootLogger=WARN
        log4j.logger.davmail=WARN
      '';
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
        work = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = ''
              Enable the work Outlook/Microsoft 365 account synced through a
              local DavMail gateway. Opt-in (off by default) since it depends
              on the tenant's Conditional Access policy allowing the client.
              Set `service.mail.work.enable = true;` once cleared with IT.
            '';
          };
          address = lib.mkOption {
            type = lib.types.str;
            default = "knut.andre.guldseth.oien@hnikt.no";
            description = "Work Outlook/Microsoft 365 email address.";
          };
          realName = lib.mkOption {
            type = lib.types.str;
            default = "Knut André Guldseth Øien";
            description = "Display name used when composing work mail.";
          };
          imapPort = lib.mkOption {
            type = lib.types.port;
            default = 1144;
            description = "Local DavMail IMAP port (kept distinct from Proton's 1143).";
          };
          smtpPort = lib.mkOption {
            type = lib.types.port;
            default = 1026;
            description = "Local DavMail SMTP port (kept distinct from Proton's 1025).";
          };
          oauthClientId = lib.mkOption {
            type = lib.types.str;
            default = "9e5f94bc-e8a4-4e73-b8be-63364c29d753";
            description = "Azure OAuth client id DavMail authenticates as.";
          };
          tenantId = lib.mkOption {
            type = lib.types.str;
            default = "common";
            description = ''
              Azure tenant for OAuth. "common" works for most orgs; set it to
              your tenant domain or tenant GUID if required.
            '';
          };
          redirectUri = lib.mkOption {
            type = lib.types.str;
            default = "http://localhost";
            description = "OAuth redirect URI; must match the chosen clientId's registration.";
          };
        };
      };

      config = lib.mkIf (cfg.enable && osConfig.environment.desktop.develop) {
        home = {
          packages =
            (with pkgs; [
              protonmail-bridge
            ])
            ++ lib.optional cfg.work.enable pkgs.davmail;
          persistence."/persist/" = {
            directories = [
              ".config/protonmail"
            ]
            ++ lib.optional cfg.work.enable ".config/davmail";
          };
        };

        sops.secrets = lib.mkIf osConfig.service.sops.enable {
          "email_user" = { };
          "email_home-passwd" = { };
          "email_work-passwd" = { };
          "email_outlook-passwd" = { };
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
                patterns = [
                  "*"
                  "!All Mail"
                ];
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
          }
          // lib.optionalAttrs cfg.work.enable {
            work = {
              address = cfg.work.address;
              userName = cfg.work.address;
              realName = cfg.work.realName;
              passwordCommand = "${pkgs.coreutils}/bin/cat ${config.sops.secrets."email_outlook-passwd".path}";
              folders = {
                inbox = "INBOX";
                drafts = "Drafts";
                sent = "Sent";
                trash = "Trash";
              };
              mbsync = {
                enable = true;
                create = "maildir";
                expunge = "both";
                patterns = [
                  "*"
                  "!Calendar"
                  "!Contacts"
                  "!Conversation History"
                  "!Tasks"
                  "!Journal"
                  "!Notes"
                  "!Outbox"
                ];
                subFolders = "Verbatim";
                extraConfig.account.AuthMechs = "LOGIN";
              };
              mu.enable = true;
              imap = {
                host = "127.0.0.1";
                port = cfg.work.imapPort;
                tls.enable = false;
              };
              smtp = {
                host = "127.0.0.1";
                port = cfg.work.smtpPort;
                tls.enable = false;
              };
              msmtp.enable = true;
            };
          };
        };
        programs = {
          mbsync.enable = true;
          msmtp.enable = true;
          mu.enable = true;
        };
        services.mbsync = {
          enable = true;
          frequency = "*:0/1";
        };

        systemd.user.services = {
          protonmail-bridge = {
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

          davmail = lib.mkIf cfg.work.enable {
            Unit = {
              Description = "DavMail Office365 gateway";
              After = [ "network.target" ];
            };
            Service = {
              Restart = "always";
              RestartSec = 10;
              ExecStartPre = pkgs.writeShellScript "davmail-init" ''
                ${pkgs.coreutils}/bin/mkdir -p ${davmailDir}
                token="$(${pkgs.gnugrep}/bin/grep '^davmail\.oauth\.' ${davmailProperties} 2>/dev/null || true)"
                ${pkgs.coreutils}/bin/cp --no-preserve=mode ${davmailConfigBase} ${davmailProperties}
                ${pkgs.coreutils}/bin/chmod u+w ${davmailProperties}
                if [ -n "$token" ]; then
                  printf '%s\n' "$token" >> ${davmailProperties}
                fi
              '';
              ExecStart = "${pkgs.davmail}/bin/davmail ${davmailProperties}";
            };
            Install.WantedBy = [ "default.target" ];
          };

          mbsync.Unit = {
            After = [
              "protonmail-bridge.service"
            ]
            ++ lib.optional cfg.work.enable "davmail.service";
            Requires = [ "protonmail-bridge.service" ];
            PartOf = [ "protonmail-bridge.service" ];
            Wants = lib.optional cfg.work.enable "davmail.service";
          };
        };
      };
    };
}
