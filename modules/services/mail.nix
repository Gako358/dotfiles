{ pkgs, config, specialArgs, ... }:
let
  cat = "${pkgs.coreutils}/bin/cat";
  certificatesFile = "${config.xdg.configHome}/protonmail/bridge-v3/cert.pem";

  passwd =
    if specialArgs.master then
      "${cat} ${config.sops.secrets."email_home-passwd".path}"
    else
      "${cat} ${config.sops.secrets."email_work-passwd".path}";
in
{
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
        passwordCommand = passwd;
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
}
