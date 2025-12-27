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
    ];

    environment.persistence."/persist" = {
      files = [
        "/var/lib/wireguard/privatekey"
      ];
    };

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
      };
    };

    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
    };

    networking.wireguard.interfaces = {
      wg0 = {
        ips = [ "10.100.0.1/24" ];
        listenPort = cfg.serverPort;
        privateKeyFile = config.sops.secrets."wg_server_private_key".path;
        peers = [
          {
            publicKey = "E8bAVuMNL2rP0FuGRYObKB9oq5oo6idz7Dmdb2B96So=";
            allowedIPs = [ "10.100.0.2/32" ];
            persistentKeepalive = 25;
          }
        ];
      };
    };
  };
}
