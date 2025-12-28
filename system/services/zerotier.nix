{ config
, lib
, pkgs
, ...
}:
let
  cfg = config.service.zerotier;
in
{
  options.service.zerotier = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable ZeroTier VPN";
    };
  };

  config = lib.mkIf cfg.enable {
    sops.secrets = lib.mkIf config.service.sops.enable {
      "zerotier_network_id" = {
        mode = "0444";
      };
    };

    services.zerotierone = {
      enable = true;
    };

    systemd.services.zerotier-join = {
      description = "Auto-join ZeroTier network from SOPS";
      after = [ "zerotierone.service" ];
      wants = [ "zerotierone.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "zerotier-join" ''
          sleep 2
          NETWORK_ID=$(cat ${config.sops.secrets."zerotier_network_id".path})

          if !  ${pkgs.zerotierone}/bin/zerotier-cli listnetworks | grep -q "$NETWORK_ID"; then
            ${pkgs.zerotierone}/bin/zerotier-cli join "$NETWORK_ID"
          fi
        '';
      };
    };

    systemd.services.zerotier-broadcast-relay = {
      description = "Relay LAN broadcasts to ZeroTier";
      after = [
        "zerotier-join.service"
        "network-online.target"
      ];
      wants = [
        "zerotier-join.service"
        "network-online.target"
      ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "10s";
        ExecStart = pkgs.writeShellScript "zt-broadcast-relay" ''
          for i in {1..30}; do
            ZT_IP=$(${pkgs.iproute2}/bin/ip -4 addr show ztxoodgtlk 2>/dev/null | ${pkgs.gnugrep}/bin/grep -oP '(?<=inet\s)\d+(\.\d+){3}')
            if [ -n "$ZT_IP" ]; then
              break
            fi
            sleep 1
          done

          if [ -z "$ZT_IP" ]; then
            echo "Failed to get ZeroTier IP"
            exit 1
          fi

          ZT_BROADCAST=$(echo "$ZT_IP" | ${pkgs.gawk}/bin/awk -F.  '{print $1"."$2"."$3".255"}')

          echo "Relaying broadcasts from 10.0.0.4:27017 to $ZT_BROADCAST:27017"

          ${pkgs.socat}/bin/socat -u UDP4-RECV:27017,bind=10.0.0.4,reuseaddr,so-broadcast UDP4-SENDTO:$ZT_BROADCAST:27017,broadcast
        '';
      };
    };

    networking.firewall = {
      allowedUDPPorts = [
        9993
        27017
      ];
      allowedTCPPorts = [ 9993 ];
      trustedInterfaces = [ "zt*" ];
    };
  };
}
