{ config
, pkgs
, host
, lib
, ...
}:
let
  cfg = config.chat.neomutt;
in
{
  options.chat.neomutt.enable = lib.mkEnableOption "neomutt";
  config = lib.mkIf cfg.enable {
    programs.mbsync.enable = true;
    programs.msmtp.enable = true;

    accounts.email.maildirBasePath = "Mail";

    home.file.mailcap = {
      target = ".config/neomutt/mailcap";
      text = ''
        application/*; mkdir -p /tmp/neomutt \; cp %s /tmp/neomutt \; xdg-open /tmp/neomutt/$(basename %s) &
      '';
    };
    programs.neomutt = {
      enable = true;
      vimKeys = true;
      sidebar = {
        enable = true;
      };
      settings = {
        mailcap_path = "~/.config/neomutt/mailcap";
      };
      macros = [
        {
          action = "<sidebar-prev><sidebar-open>";
          key = "\\Ck";
          map = [ "index" "pager" ];
        }
        {
          action = "<sidebar-next><sidebar-open>";
          key = "\\Cj";
          map = [ "index" "pager" ];
        }
      ];
      extraConfig = ''
        shutdown-hook 'echo `rm -f /tmp/neomutt/*`'
      '';
    };
    services.mbsync.enable = true;
  };
}

