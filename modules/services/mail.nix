{ pkgs, config, ... }:
let
  cat = "${pkgs.coreutils}/bin/cat";
in
{
  home.packages = with pkgs; [
    protonmail-bridge
  ];

  accounts.email = {
    maildirBasePath = "Mail";
    accounts = {
      personal = {
        primary = true;
        address = "${cat} ${config.sops.secrets."email-private".path}";
        aliases = [
          "${cat} ${config.sops.secrets."email-alias-private".path}"
          "${cat} ${config.sops.secrets."email-alias-service".path}"
          "${cat} ${config.sops.secrets."email-alias-social".path}"
        ];
        userName = "${cat} ${config.sops.secrets."email-private".path}";
        passwordCommand = "${cat} ${config.sops.secrets."email-passwd".path}";
        realName = "merrinx";
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          patterns = [ "* '!All Mail'" ];
        };
        mu.enable = true;
        imap = {
          host = "127.0.0.1";
          port = 1143;
          tls = {
            enable = true;
            useStartTls = true;
            # Use protonmail-bride -c and cert export
            certificatesFile = "${config.xdg.configHome}/protonmail/bridge-v3/cert.pem";
          };
        };
      };
    };
  };

  # programs.mbsync.enable = true;
  # services.mbsync = {
  #   enable = true;
  #   frequency = "*:0/1";
  # };

  # systemd.user.services.protonmail-bridge = {
  #   Unit = {
  #     Description = "Proton Mail Bridge";
  #     After = [ "network.target" ];
  #   };
  #   Service = {
  #     Restart = "always";
  #     RestartSec = 5;
  #     ExecStart = "${pkgs.protonmail-bridge}/bin/protonmail-bridge --no-window --noninteractive --log-level debug";
  #   };
  #   Install.WantedBy = [ "default.target" ];
  # };
}
