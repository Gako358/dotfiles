_: {
  flake.nixosModules.services-eca-access =
    { config, lib, ... }:
    let
      cfg = config.service.ecaAccess;
      remoteActive = cfg.enable && cfg.remote.enable;
      metricsActive = cfg.enable && cfg.metrics.enable;
      exposureActive = remoteActive || metricsActive;
      loopbackHttpEndpoint =
        builtins.match "http://127\\.0\\.0\\.1:(6553[0-5]|655[0-2][0-9]|65[0-4][0-9]{2}|6[0-4][0-9]{3}|[1-5][0-9]{4}|[1-9][0-9]{0,3})/v1/metrics" cfg.metrics.endpoint
        != null;
      grafanaPortOverlapsEcaRange = cfg.grafana.servePort >= 7777 && cfg.grafana.servePort <= 7796;
    in
    {
      options.service.ecaAccess = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = config.environment.server.enable;
          defaultText = lib.literalExpression "config.environment.server.enable";
          description = "Enable ECA access integration for server hosts.";
        };

        tailnetHost = lib.mkOption {
          type = lib.types.nullOr lib.types.nonEmptyStr;
          default = null;
          description = "Tailnet hostname advertised by ECA and Grafana access paths.";
        };

        remote = {
          enable = lib.mkEnableOption "the ECA remote access path";

          bindHost = lib.mkOption {
            type = lib.types.str;
            default = "127.0.0.1";
            description = "Loopback address used by the ECA remote listener.";
          };
        };

        metrics = {
          enable = lib.mkEnableOption "the ECA metrics path";

          endpoint = lib.mkOption {
            type = lib.types.str;
            default = "http://127.0.0.1:4318/v1/metrics";
            description = "Loopback OTLP HTTP metrics endpoint.";
          };
        };

        prometheus.retentionTime = lib.mkOption {
          type = lib.types.nonEmptyStr;
          default = "7d";
          description = "Prometheus data retention period for ECA metrics.";
        };

        grafana.servePort = lib.mkOption {
          type = lib.types.port;
          default = 443;
          description = "Tailscale Serve port reserved for Grafana.";
        };
      };

      config = lib.mkMerge [
        {
          assertions = [
            {
              assertion = !exposureActive || config.service.tailscale.enable;
              message = "active ECA access paths require service.tailscale.enable";
            }
            {
              assertion = !exposureActive || config.service.sops.enable;
              message = "active ECA access paths require service.sops.enable";
            }
            {
              assertion = !exposureActive || cfg.tailnetHost != null;
              message = "service.ecaAccess.tailnetHost must be set when remote access or metrics is active";
            }
            {
              assertion = !remoteActive || cfg.remote.bindHost == "127.0.0.1";
              message = "service.ecaAccess.remote.bindHost must be 127.0.0.1";
            }
            {
              assertion = !metricsActive || loopbackHttpEndpoint;
              message = "service.ecaAccess.metrics.endpoint must be an HTTP loopback endpoint";
            }
            {
              assertion = !metricsActive || !grafanaPortOverlapsEcaRange;
              message = "service.ecaAccess.grafana.servePort must not overlap ECA ports 7777-7796";
            }
          ];
        }

        (lib.mkIf metricsActive {
          sops.secrets = {
            "grafana/admin_password" = {
              owner = "grafana";
              group = "grafana";
              mode = "0400";
            };
            "grafana/secret_key" = {
              owner = "grafana";
              group = "grafana";
              mode = "0400";
            };
          };

          services = {
            opentelemetry-collector = {
              enable = true;
              settings = {
                receivers.otlp.protocols.http.endpoint = "127.0.0.1:4318";
                exporters.prometheus.endpoint = "127.0.0.1:9464";
                service = {
                  telemetry.metrics.level = "none";
                  pipelines.metrics = {
                    receivers = [ "otlp" ];
                    exporters = [ "prometheus" ];
                  };
                };
              };
            };

            prometheus = {
              enable = true;
              listenAddress = "127.0.0.1";
              port = 9090;
              retentionTime = cfg.prometheus.retentionTime;
              scrapeConfigs = [
                {
                  job_name = "eca-otel";
                  static_configs = [
                    { targets = [ "127.0.0.1:9464" ]; }
                  ];
                }
              ];
            };

            grafana = {
              enable = true;
              settings = {
                server = {
                  http_addr = "127.0.0.1";
                  http_port = 3000;
                };
                security = {
                  admin_password = "$__file{${config.sops.secrets."grafana/admin_password".path}}";
                  secret_key = "$__file{${config.sops.secrets."grafana/secret_key".path}}";
                };
                "auth.anonymous".enabled = false;
                users.allow_sign_up = false;
              };
              provision = {
                enable = true;
                datasources.settings = {
                  apiVersion = 1;
                  datasources = [
                    {
                      name = "Prometheus";
                      type = "prometheus";
                      access = "proxy";
                      url = "http://127.0.0.1:9090";
                      editable = false;
                      isDefault = true;
                    }
                  ];
                };
              };
            };
          };

          environment.persistence."/persist".directories = [
            {
              directory = "/var/lib/prometheus2";
              user = "prometheus";
              group = "prometheus";
            }
            {
              directory = "/var/lib/grafana";
              user = "grafana";
              group = "grafana";
            }
          ];
        })
      ];
    };
}
