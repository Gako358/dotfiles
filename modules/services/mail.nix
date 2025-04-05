{ pkgs, config, ... }:
let
  cat = "${pkgs.coreutils}/bin/cat";
in
{
  home.packages = with pkgs; [
    protonmail-bridge
  ];

  accounts.email = {
    maildirBasePath = "Mail";
    accounts = {
      personal = {
        primary = true;
        # TODO: Figure out why mu wont allow aliases
        # aliases = [
        #   "${cat} ${config.sops.secrets.email-alias-private.path}"
        #   "${cat} ${config.sops.secrets.email-alias-service.path}"
        #   "${cat} ${config.sops.secrets.email-alias-social.path}"
        # ];
        address = "merrinx@proton.me";
        userName = "merrinx@proton.me";
        passwordCommand = "${cat} ${config.sops.secrets.email-passwd.path}";
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
          patterns = [ "*" ];
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
            certificatesFile = "${config.xdg.configHome}/protonmail/bridge-v3/cert.pem";
          };
        };
        smtp = {
          host = "127.0.0.1";
          port = 1025;
          tls = {
            enable = true;
            useStartTls = true;
            certificatesFile = "${config.xdg.configHome}/protonmail/bridge-v3/cert.pem";
          };
        };
        msmtp.enable = true;
      };
    };
  };

  programs.mbsync.enable = true;
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
      ExecStart = "${pkgs.protonmail-bridge}/bin/protonmail-bridge --no-window --noninteractive --log-level debug";
    };
    Install.WantedBy = [ "default.target" ];
  };
}
