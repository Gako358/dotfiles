_: {
  flake.nixosModules.services-ollama =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.service.ollama;
    in
    {
      options.service.ollama = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Run a local Ollama server for offline LLMs";
        };

        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.ollama-vulkan;
          defaultText = lib.literalExpression "pkgs.ollama-vulkan";
          description = ''
            Backend the server runs on. Vulkan is the default because ROCm no
            longer ships kernels for every RDNA generation, while the Vulkan
            backend works on any Mesa-supported GPU. Fall back to
            'pkgs.ollama-cpu' if the GPU backend misbehaves, or
            'pkgs.ollama-rocm' (plus services.ollama.rocmOverrideGfx) on a
            ROCm-supported card.
          '';
        };

        models = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ "qwen3:8b" ];
          example = [
            "qwen3:8b"
            "qwen2.5-coder:7b"
          ];
          description = ''
            Models pulled once the server is up. The first entry is what ECA
            uses for its 'local' agent. Browse https://ollama.com/library.
          '';
        };

        contextLength = lib.mkOption {
          type = lib.types.ints.positive;
          default = 16384;
          description = ''
            Context window every model is loaded with. Ollama defaults to 4096,
            which is too small for agentic tool use. Raising it costs VRAM for
            the KV cache, so it is capped well below what the models support.
          '';
        };

        port = lib.mkOption {
          type = lib.types.port;
          default = 11434;
          description = "Port the server listens on, loopback only";
        };
      };

      config = lib.mkIf cfg.enable {
        services.ollama = {
          enable = true;
          inherit (cfg) package port;
          user = "ollama";
          group = "ollama";
          loadModels = cfg.models;
          environmentVariables = {
            OLLAMA_CONTEXT_LENGTH = toString cfg.contextLength;
            OLLAMA_FLASH_ATTENTION = "1";
            OLLAMA_KV_CACHE_TYPE = "q8_0";
          };
        };

        systemd.services.ollama.serviceConfig = {
          # Upstream sets DynamicUser even with a static user, which moves the
          # state directory to /var/lib/private/ollama and breaks the
          # impermanence bind mount on /var/lib/ollama.
          DynamicUser = lib.mkForce false;
          # Upstream lists the models dir in ReadWritePaths but never creates
          # it, so the mount namespace fails on a fresh state directory.
          StateDirectory = lib.mkForce [
            "ollama"
            "ollama/models"
          ];
        };

        environment.systemPackages = [ cfg.package ];

        environment.persistence."/persist".directories = [
          {
            directory = "/var/lib/ollama";
            user = "ollama";
            group = "ollama";
            mode = "0700";
          }
        ];
      };
    };
}
