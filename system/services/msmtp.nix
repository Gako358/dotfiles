{ pkgs, config, ... }:
let
  cat = "${pkgs.coreutils}/bin/cat";

in
{
  programs.msmtp = {
    enable = true;
    accounts.default = {
      host = "127.0.0.1";
      port = 1025;
      from = "${cat} ${config.sops.secrets."email_username".path}";
      user = "${cat} ${config.sops.secrets."email_username".path}";
      passwordeval = "${cat} ${config.sops.secrets."email_home-passwd".path}";
    };
  };
}
