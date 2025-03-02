{ config
, ...
}:
{
  accounts.email.accounts.work = {
    primary = false;
    flavor = "outlook.office365.com";

    realName = "Knut Øien";
    address = "gako358@outlook.com";
    userName = "gako358@outlook.com";
    passwordCommand = "pass email/gako358 | head -1";

    maildir.path = "work";

    folders = {
      inbox = "inbox";
      sent = "Sent";
      drafts = "Drafts";
      trash = "Deleted";
    };

    imap = {
      host = "outlook.office365.com";
      port = 993;
      tls.enable = true;
    };

    himalaya = {
      inherit (config.programs.himalaya) enable;
      settings = {
        backend = {
          type = "imap";
          server = "outlook.office365.com";
          port = 993;
          login = "gako358@outlook.com";
          insecure = false;
        };
      };
    };
  };
}
