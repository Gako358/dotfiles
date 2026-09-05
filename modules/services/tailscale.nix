_: {
  flake.nixosModules.services-tailscale =
    { config, lib, ... }:
    let
      cfg = config.service.tailscale;
      operatorIsNormalUser =
        cfg.operator != null
        && builtins.hasAttr cfg.operator config.users.users
        && config.users.users.${cfg.operator}.isNormalUser;
    in
    {
      options.service.tailscale = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
        };
        operator = lib.mkOption {
          type = lib.types.nullOr lib.types.nonEmptyStr;
          default = null;
        };
        openFirewall = lib.mkOption {
          type = lib.types.bool;
          default = false;
        };
      };

      config = lib.mkIf cfg.enable {
        assertions = [
          {
            assertion = cfg.operator == null || operatorIsNormalUser;
            message = "service.tailscale.operator must name a declared normal user";
          }
        ];

        services.tailscale = {
          enable = true;
          inherit (cfg) openFirewall;
          useRoutingFeatures = "none";
        };

        environment.persistence."/persist".directories = [ "/var/lib/tailscale" ];

        security.sudo.extraRules = lib.mkIf (cfg.operator != null) [
          {
            users = [ cfg.operator ];
            commands = [
              {
                command = "${config.services.tailscale.package}/bin/tailscale up --operator=${cfg.operator}";
                options = [ "NOPASSWD" ];
              }
            ];
          }
        ];
      };
    };
}
