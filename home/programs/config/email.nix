{ pkgs, config, ... }:
let

  preNewHook = pkgs.writeShellScriptBin "notmuch-pre-new" ''
    folder=$(basename $(pwd))
    ${pkgs.isync}/bin/mbsync -a
    tags="+inbox -unread"
  '';

  postnewHook = pkgs.writeShellScriptBin "notmuch-post-new" ''
    echo "Hello from $0"

    notmuch tag --batch <<EOF
    +personal -- tag:unread folder:/prvAcc/
    +work -- tag:unread folder:/gitAcc/
    +spam -inbox -- folder:/Spam/
    -inbox -- tag:sent and tag:inbox
    EOF
  '';

in
{
  accounts.email = {
    maildirBasePath = "Documents/mails";
    accounts.gitAcc = {
      address = "gako358@outlook.com";
      userName = "gako358@outlook.com";
      realName = "Knut Oien";
      passwordCommand = "${pkgs.gnupg}/bin/gpg -q --for-your-eyes-only --no-tty --exit-on-status-write-error --batch --passphrase-file ${config.home.homeDirectory}/Documents/classified/git.pass -d ${config.home.homeDirectory}/Sources/agentx/git.pass.gpg";
      imap.host = "outlook.office365.com";
      smtp.host = "smtp.office365.com";
      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        patterns = [ "INBOX" "INBOX.Sent" "INBOX.Drafts" "INBOX.Trash" "INBOX.Junk" ];
        extraConfig = {
          channel = {
            Sync = "All";
          };
          account = {
            Timeout = 120;
            PipelineDepth = 1;
          };
        };
      };
      msmtp.enable = true;
      smtp.port = 587;
      notmuch.enable = true;
      primary = true;
    };
    accounts.prvAcc = {
      address = "knutago1@outlook.com";
      userName = "knutago1@outlook.com";
      realName = "Knut Oien";
      passwordCommand = "${pkgs.gnupg}/bin/gpg -q --for-your-eyes-only --no-tty --exit-on-status-write-error --batch --passphrase-file ${config.home.homeDirectory}/Documents/classified/prv.pass -d ${config.home.homeDirectory}/Sources/agentx/prv.pass.gpg";
      imap.host = "outlook.office365.com";
      smtp.host = "smtp.office365.com";
      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        patterns = [ "INBOX" "INBOX.Sent" "INBOX.Drafts" "INBOX.Trash" "INBOX.Junk" ];
        extraConfig = {
          channel = {
            Sync = "All";
          };
          account = {
            Timeout = 120;
            PipelineDepth = 1;
          };
        };
      };
      msmtp.enable = true;
      smtp.port = 587;
      notmuch.enable = true;
    };
  };

  programs.msmtp.enable = true;
  programs.mbsync.enable = true;
  programs.notmuch = {
    enable = true;
    hooks.preNew = "${preNewHook}/bin/notmuch-pre-new";
    hooks.postNew = "${postnewHook}/bin/notmuch-post-new";
  };

  services.mbsync = {
    enable = true;
    frequency = "*:0/3";
  };
  home.packages = with pkgs; [
    notmuch
    isync
  ];
}
