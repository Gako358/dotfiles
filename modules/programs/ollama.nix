_: {
  flake.homeModules.programs-eca =
    {
      osConfig,
      config,
      lib,
      options,
      ...
    }:
    let
      cfg = osConfig.service.ollama;
      model = lib.head cfg.models;
      emacsAvailable = lib.hasAttrByPath [ "programs" "merrinx-emacs" ] options;
      emacsEnabled = emacsAvailable && config.programs.merrinx-emacs.enable;
      ollamaSettings = {
        # Local models spend minutes on prompt eval before the first token,
        # which the 120s default reads as a dead stream.
        streamIdleTimeoutSeconds = 600;

        providers.ollama = {
          url = "http://127.0.0.1:${toString cfg.port}";
          models = lib.genAttrs cfg.models (_: {
            limit = {
              context = cfg.contextLength;
              output = 8192;
            };
            cost = {
              input = 0;
              output = 0;
            };
          });
        };

        agent.local = {
          # quoted: `inherit` is a Nix keyword, ECA wants it as a JSON key
          "inherit" = "code";
          defaultModel = "ollama/${model}";
        };

        extraConfigs = [ "${config.home.homeDirectory}/.config/eca/config.local.json" ];
      };
    in
    {
      config = lib.mkIf (cfg.enable && cfg.models != [ ]) (
        lib.mkMerge [
          (lib.optionalAttrs emacsAvailable {
            programs.merrinx-emacs.eca.extraSettings = lib.mkIf (
              emacsEnabled && config.programs.merrinx-emacs.eca.settings != null
            ) ollamaSettings;
          })
          {
            xdg.configFile."eca/config.json".text = lib.mkIf (!emacsEnabled) (builtins.toJSON ollamaSettings);
          }
        ]
      );
    };
}
