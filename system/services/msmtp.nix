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
      from = "${cat} ${config.sops.secrets.email-user.path}";
      user = "${cat} ${config.sops.secrets.email-user.path}";
      tls = true;
      tls_starttls = true;
      tls_trust_file = "/etc/ssl/certs/ca-certificates.crt";
      passwordeval = "${cat} ${config.sops.secrets.email-passwd.path}";
    };
  };
}
