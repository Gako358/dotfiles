{ pkgs, config, ... }:
let
  cat = "${pkgs.coreutils}/bin/cat";

in
{
  programs.msmtp = {
    enable = true;
    accounts.default = {
      auth = true;
      host = "127.0.0.1";
      port = 1025;
      from = "${cat} ${config.sops.secrets."email_user".path}";
      user = "${cat} ${config.sops.secrets."email_user".path}";
      tls = true;
      tls_starttls = true;
      passwordeval = "${cat} ${config.sops.secrets."email_master-passwd".path}";
    };
  };
}
