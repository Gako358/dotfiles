{ pkgs, config, ... }:
let
  cat = "${pkgs.coreutils}/bin/cat";

in
{
  # Copy the certificate to /etc
  environment.etc."protonmail-cert.pem" = {
    source = "/home/merrinx/.config/protonmail/bridge-v3/cert.pem";
    mode = "0444";
  };

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
      tls_trust_file = "/etc/protonmail-cert.pem";
      passwordeval = "${cat} ${config.sops.secrets."email_master-passwd".path}";
    };
  };
}
