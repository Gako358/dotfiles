{ config
, ...
}:
{
  accounts.email.accounts.personal = {
    primary = true;
    flavor = "outlook.office365.com";

    realName = "Knut Øien";
    address = "knutago1@outlook.com";
    userName = "knutago1@outlook.com";
    passwordCommand = "pass email/knutago1 | head -1";

    maildir.path = "personal";

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
          login = "knutago1@outlook.com";
          insecure = false;
        };
      };
    };
  };
}
