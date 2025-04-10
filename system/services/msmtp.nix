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
      from = "${cat} ${config.sops.secrets."email_username".path}";
      user = "${cat} ${config.sops.secrets."email_username".path}";
      tls = false;
      tls_starttls = false;
      passwordeval = "${cat} ${config.sops.secrets."email_home-passwd".path}";
    };
  };
}
