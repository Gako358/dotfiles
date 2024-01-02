{ pkgs, config, ... }:
let

  preNewHook = pkgs.writeShellScriptBin "notmuch-pre-new" ''
    folder=$(basename $(pwd))
    tags="+inbox -unread"

    notmuch new
    notmuch tag $tags -- folder:$folder/*

    echo "Delete discarded drafts..."
    notmuch search --output=files --format=text0 -- tag:deleted and tag:draft | xargs -0 rm -fv

    echo "Delete old discarded messages..."
    notmuch search --output=files --format=text0 -- tag:deleted date:..6w | xargs -0 rm -fv
    notmuch search --output=files --format=text0 -- tag:spam date:..3w | xargs -0 rm -fv

    echo "Move deleted messages out of inbox..."
    notmuch search --output=files -- tag:deleted and folder:$folder/Inbox | xargs -I {} mv {} $folder/Trash

    echo "Move spam messages out of inbox..."
    notmuch search --output=files -- tag:spam and folder:$folder/Inbox | xargs -I {} mv {} $folder/Spam

    echo "Move archived messages out of inbox..."
    notmuch search --output=files -- -tag:inbox and folder:$folder/Inbox | xargs -I {} mv {} $folder/Archive

    echo "Move non-spam out of spam folder..."
    notmuch search --output=files -- not tag:spam and folder:$folder/Spam | xargs -I {} mv {} $folder/Inbox
  '';

  postNewHook = pkgs.writeShellScriptBin "notmuch-post-new" ''
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
    hooks.postNew = "${postNewHook}/bin/notmuch-post-new";
  };

  services.mbsync = {
    enable = true;
    frequency = "*:0/5";
  };
}
