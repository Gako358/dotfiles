{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.services.proxySudoers;
  proxy_server = "www-proxy.helsenord.no";
  proxy_port = "8080";
  no_proxy = "localhost,127.0.0.1,*.localhost,*.helsenord.no";
  NO_PROXY = "*.helsenord.no,packagemanager.helsenord.no";
  setupEnv = pkgs.writeShellScript "setupEnv" ''
    # Check if the script is run as root
    if [ "$(id -u)" != "0" ]; then
      echo "This script must be run as root."
      exit 1
    fi

    # Create the directory for the Nix daemon service override
    mkdir -p /run/systemd/system/nix-daemon.service.d/

    # Create the override.conf file with the appropriate proxy settings
    cat << EOF >/run/systemd/system/nix-daemon.service.d/override.conf
    [Service]
    Environment="http_proxy=http://${proxy_server}:${proxy_port}"
    Environment="https_proxy=http://${proxy_server}:${proxy_port}"
    Environment="ftp_proxy=http://${proxy_server}:${proxy_port}"
    Environment="no_proxy=${no_proxy}"
    Environment="HTTP_PROXY=http://${proxy_server}:${proxy_port}"
    Environment="HTTPS_PROXY=http://${proxy_server}:${proxy_port}"
    Environment="FTP_PROXY=http://${proxy_server}:${proxy_port}"
    Environment="NO_PROXY=${NO_PROXY}"
    EOF

    # Reload the systemd daemon and restart the Nix daemon
    systemctl daemon-reload
    systemctl restart nix-daemon.service

    # Print success message
    echo "Proxy settings and SSL certificate applied successfully for Nix daemon."
  '';
in {
  options = {
    services.proxySudoers = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable the sudoers service.";
      };
    };
  };
  config = mkIf (cfg.enable && (config.desktop.environment == "wsl")) {
    systemd.services.proxySudoers = {
      description = "Setup proxy settings for Nix daemon";
      wantedBy = ["multi-user.target"];
      after = ["network.target"];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${setupEnv}";
        RemainAfterExit = true;
      };
    };
  };
}
