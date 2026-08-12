_: {
  flake.nixosModules.services-protonvpn =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.service.protonvpn;

      endpointParts = lib.splitString ":" cfg.endpoint;
      endpointHost = lib.head endpointParts;
      endpointPort = lib.last endpointParts;

      systemctl = "${pkgs.systemd}/bin/systemctl";

      vpnCommand = pkgs.writeShellScriptBin "vpn" ''
        unit="wg-quick-${cfg.interface}.service"

        case "''${1:-status}" in
          up)
            ${systemctl} start "$unit" && echo "vpn up"
            ;;
          down)
            ${systemctl} stop "$unit"
            ${lib.optionalString cfg.killSwitch "${systemctl} stop protonvpn-killswitch.service"}
            echo "vpn down"
            ;;
          toggle)
            if ${systemctl} is-active --quiet "$unit"; then
              exec "$0" down
            else
              exec "$0" up
            fi
            ;;
          status)
            if ${systemctl} is-active --quiet "$unit"; then
              echo "vpn up (${cfg.interface} -> ${cfg.endpoint})"
              echo "exit ip: $(${pkgs.curl}/bin/curl -4 -s --max-time 5 https://ifconfig.me || echo unknown)"
            else
              echo "vpn down"
            fi
            ;;
          *)
            echo "usage: vpn [up|down|toggle|status]" >&2
            exit 1
            ;;
        esac
      '';

      killSwitchRules = pkgs.writeText "protonvpn-killswitch.nft" ''
        table inet protonvpn-killswitch {}
        delete table inet protonvpn-killswitch

        table inet protonvpn-killswitch {
          chain output {
            type filter hook output priority filter; policy drop;

            oifname { "lo", "${cfg.interface}" } accept

            ip daddr ${endpointHost} udp dport ${endpointPort} accept

            ip daddr {
              10.0.0.0/8,
              172.16.0.0/12,
              192.168.0.0/16,
              169.254.0.0/16,
              224.0.0.0/4,
              255.255.255.255
            } accept

            ip6 daddr { fe80::/10, fc00::/7, ff00::/8 } accept
          }
        }
      '';
    in
    {
      options.service.protonvpn = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable ProtonVPN as a WireGuard tunnel";
        };

        interface = lib.mkOption {
          type = lib.types.str;
          default = "proton0";
          description = "WireGuard interface name";
        };

        autostart = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Bring the tunnel up at boot. When false, toggle it with
            'systemctl start|stop wg-quick-<interface>'.
          '';
        };

        killSwitch = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Drop every packet that does not leave through the tunnel, apart from
            loopback, LAN and the handshake with the VPN endpoint. Fails closed:
            if the tunnel never comes up there is no internet until
            'systemctl stop wg-quick-<interface>' is run.
          '';
        };

        endpoint = lib.mkOption {
          type = lib.types.str;
          example = "146.70.179.50:51820";
          description = "Endpoint from the WireGuard config downloaded from ProtonVPN";
        };

        publicKey = lib.mkOption {
          type = lib.types.str;
          example = "d4Yy3Pd/ZFy8UlBS6IdvIfV3IjJlXd93pDvT2LTPMhE=";
          description = "ProtonVPN server public key";
        };

        address = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            "10.2.0.2/32"
            "2a07:b944::2:2/128"
          ];
          description = "Interface addresses, taken from the downloaded config";
        };

        dns = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            "10.2.0.1"
            "2a07:b944::2:1"
          ];
          description = "DNS servers pushed while the tunnel is up";
        };

        allowedIPs = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            "0.0.0.0/0"
            "::/0"
          ];
          description = ''
            Traffic routed through the tunnel. Defaults to everything;
            ::/0 is included so IPv6 cannot leak around the tunnel.
          '';
        };

        secret = lib.mkOption {
          type = lib.types.str;
          default = "protonvpn/${config.networking.hostName}";
          defaultText = lib.literalExpression ''"protonvpn/''${config.networking.hostName}"'';
          description = "Key in the sops file holding this host's WireGuard private key";
        };
      };

      config = lib.mkIf cfg.enable {
        assertions = [
          {
            assertion = config.service.sops.enable;
            message = "service.protonvpn requires service.sops.enable for the WireGuard private key.";
          }
          {
            assertion = !cfg.killSwitch || lib.length endpointParts == 2;
            message = "service.protonvpn.killSwitch needs service.protonvpn.endpoint as <ipv4>:<port>.";
          }
        ];

        sops.secrets.${cfg.secret}.mode = "0400";

        environment.systemPackages = [
          pkgs.wireguard-tools
          vpnCommand
        ]
        ++ lib.optional cfg.killSwitch pkgs.nftables;

        security.polkit.extraConfig = ''
          polkit.addRule(function(action, subject) {
            if (action.id == "org.freedesktop.systemd1.manage-units" &&
                subject.isInGroup("wheel") &&
                (action.lookup("unit") == "wg-quick-${cfg.interface}.service" ||
                 action.lookup("unit") == "protonvpn-killswitch.service")) {
              return polkit.Result.YES;
            }
          });
        '';

        networking.networkmanager.unmanaged = [ "interface-name:${cfg.interface}" ];

        networking.wg-quick.interfaces.${cfg.interface} = {
          inherit (cfg) address autostart dns;
          privateKeyFile = config.sops.secrets.${cfg.secret}.path;

          peers = [
            {
              inherit (cfg) endpoint publicKey allowedIPs;
              persistentKeepalive = 25;
            }
          ];
        };

        systemd.services = lib.mkIf cfg.killSwitch {
          protonvpn-killswitch = {
            description = "ProtonVPN kill switch";
            partOf = [ "wg-quick-${cfg.interface}.service" ];

            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
              ExecStart = "${pkgs.nftables}/bin/nft -f ${killSwitchRules}";
              ExecStop = "${pkgs.nftables}/bin/nft delete table inet protonvpn-killswitch";
            };
          };

          "wg-quick-${cfg.interface}" = {
            requires = [ "protonvpn-killswitch.service" ];
            after = [ "protonvpn-killswitch.service" ];
          };
        };
      };
    };
}
