{ config
, lib
, pkgs
, ...
}:
let
  cfg = config.service.wireguard;
in
{
  options.service.wireguard = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable WireGuard VPN server";
    };
    serverPort = lib.mkOption {
      type = lib.types.port;
      default = 51820;
      description = "WireGuard server listen port";
    };
    externalInterface = lib.mkOption {
      type = lib.types.str;
      default = "eth0";
      description = "External network interface (check with 'ip a')";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.wireguard-tools
      pkgs.socat
    ];

    sops.secrets = lib.mkIf config.service.sops.enable {
      "wg_server_private_key" = {
        path = "/var/lib/wireguard/privatekey";
        mode = "0400";
      };
    };

    networking = {
      nat = {
        enable = true;
        inherit (cfg) externalInterface;
        internalInterfaces = [ "wg0" ];
      };

      firewall = {
        allowedUDPPorts = [ cfg.serverPort ];
        trustedInterfaces = [ "wg0" ];
      };
    };

    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.ipv4.conf.all.bc_forwarding" = 1;
    };

    networking.wireguard.interfaces = {
      wg0 = {
        ips = [ "10.100.0.1/24" ];
        listenPort = cfg.serverPort;
        privateKeyFile = config.sops.secrets."wg_server_private_key".path;

        postSetup = ''
          ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT
          ${pkgs.iptables}/bin/iptables -A FORWARD -o wg0 -j ACCEPT
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o ${cfg.externalInterface} -j MASQUERADE
        '';

        postShutdown = ''
          ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT
          ${pkgs.iptables}/bin/iptables -D FORWARD -o wg0 -j ACCEPT
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o ${cfg.externalInterface} -j MASQUERADE
        '';

        peers = [
          {
            publicKey = "E8bAVuMNL2rP0FuGRYObKB9oq5oo6idz7Dmdb2B96So=";
            allowedIPs = [ "10.100.0.2/32" ];
            persistentKeepalive = 25;
          }
        ];
      };
    };

    systemd.services.totalwar-broadcast-relay = {
      description = "Total War LAN broadcast relay";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "wireguard-wg0.service"
      ];
      wants = [ "wireguard-wg0.service" ];

      serviceConfig = {
        Type = "forking";
        ExecStart = pkgs.writeShellScript "start-relay" ''
          # Relay broadcasts from LAN to VPN for common Total War ports
          for port in 2300 2302 2303 2350; do
            ${pkgs.socat}/bin/socat UDP4-RECVFROM:$port,broadcast,fork UDP4-SENDTO:10.100.0.255:$port,broadcast &
          done
        '';
        ExecStop = "${pkgs.procps}/bin/pkill -f 'socat.*2300|socat.*2302|socat.*2303|socat.*2350'";
        Restart = "on-failure";
        RestartSec = "10s";
      };
    };
  };
}
