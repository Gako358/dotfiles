{ pkgs, config, specialArgs, ... }:
let
  cat = "${pkgs.coreutils}/bin/cat";

  passwd =
    if specialArgs.master then
      "${cat} ${config.sops.secrets."email_keys/master-passwd".path}"
    else
      "${cat} ${config.sops.secrets."email_keys/work-passwd".path}";

in
{
  programs.msmtp = {
    enable = true;
    accounts.default = {
      auth = true;
      host = "127.0.0.1";
      port = 1025;
      from = "${cat} ${config.sops.secrets."email_keys/user".path}";
      user = "${cat} ${config.sops.secrets."email_keys/user".path}";
      tls = true;
      tls_starttls = true;
      tls_trust_file = "/etc/ssl/certs/ca-certificates.crt";
      passwordeval = passwd;
    };
  };
}
