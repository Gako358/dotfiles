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

    networking.firewall = {
      allowedUDPPorts = [ 9993 ];
      allowedTCPPorts = [ 9993 ];
      trustedInterfaces = [ "zt*" ];
    };
  };
}
