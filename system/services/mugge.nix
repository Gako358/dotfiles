{ pkgs
, inputs
, ...
}:
let
  mugge-azure-wrapped = pkgs.symlinkJoin {
    name = "mugge-azure-wrapped";
    paths = [ inputs.mugge.packages.${pkgs.system}.mugge-azure ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/mugge-azure \
        --prefix PATH : ${
          pkgs.lib.makeBinPath [
            pkgs.libnotify
            pkgs.openssh
            pkgs.openssl
            pkgs.git
          ]
        }
    '';
  };

  mugge-notifier = pkgs.writeShellScript "mugge-notifier" ''
    LOG_FILE="/run/user/$(id -u)/mugge-azure.log"

    touch "$LOG_FILE"

    tail -f "$LOG_FILE" | while IFS= read -r line; do
      if [[ "$line" =~ \[([0-9]{2}:[0-9]{2}:[0-9]{2})\][[:space:]]✓[[:space:]]([^:]+):[[:space:]](.*) ]]; then
        TIME="''${BASH_REMATCH[1]}"
        USER="''${BASH_REMATCH[2]}"
        MESSAGE="''${BASH_REMATCH[3]}"
      fi
    done
  '';

  azure-mugge = pkgs.writeShellScriptBin "azure-mugge" ''
    SOCKET="/run/user/$(id -u)/mugge-azure.dtach"

    if ! systemctl --user is-active --quiet mugge-azure.service; then
      echo "Starting Mugge Azure service..."
      systemctl --user start mugge-azure.service
      sleep 2
    fi

    echo "Attaching to Mugge Azure chat..."
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  ⚠️  To safely detach: Press Ctrl+\ "
    echo "  ❌  Ctrl+C or closing terminal will NOT stop the chat"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""

    trap 'echo -e "\n\n>>> Detaching from chat (service continues running)..."; exit 0' INT
    ${pkgs.dtach}/bin/dtach -a "$SOCKET" -E
  '';

  mugge-service-wrapper = pkgs.writeShellScript "mugge-service-wrapper" ''
    trap "" INT TERM HUP
    exec ${mugge-azure-wrapped}/bin/mugge-azure 2>&1 | tee -a /run/user/$(id -u)/mugge-azure.log
  '';
in
{
  environment.systemPackages = [
    mugge-azure-wrapped
    azure-mugge
    pkgs.dtach
  ];

  systemd.user.services.mugge-azure = {
    description = "Mugge Azure Chat Client";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];

    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.coreutils}/bin/nohup ${pkgs.util-linux}/bin/setsid ${pkgs.dtach}/bin/dtach -n /run/user/%U/mugge-azure.dtach ${mugge-service-wrapper} >/dev/null 2>&1 &'";
      ExecStartPost = "${pkgs.coreutils}/bin/sleep 2";
      Restart = "on-failure";
      RestartSec = 10;
      KillMode = "process";
      SendSIGKILL = "no";
    };

    wantedBy = [ "default.target" ];
  };

  systemd.user.services.mugge-notifier = {
    description = "Mugge Chat Notification Service";
    after = [ "mugge-azure.service" ];
    requires = [ "mugge-azure.service" ];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${mugge-notifier}";
      Restart = "always";
      RestartSec = 5;
      Environment = [
        "DISPLAY=:0"
        "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%U/bus"
      ];
    };

    wantedBy = [ "default.target" ];
  };

  programs.fish.shellAliases = {
    am = "azure-mugge";
    mugge = "azure-mugge";
    mugge-stop = "systemctl --user stop mugge-azure mugge-notifier";
    mugge-restart = "systemctl --user restart mugge-azure mugge-notifier";
    mugge-status = "systemctl --user status mugge-azure";
  };
}
