_: {
  flake.homeModules.programs-emacs =
    {
      lib,
      config,
      osConfig,
      inputs,
      ...
    }:
    let
      inherit (osConfig.environment) desktop;
    in
    {
      imports = [ inputs.emacs-flake.homeModules.emacs ];

      config = lib.mkMerge [
        (lib.mkIf (desktop.enable && desktop.develop) {
          programs.merrinx-emacs.enable = true;

          programs.fish.shellAliases = {
            vim = "emacs-minimal";
            vi = "emacs-minimal";
          };

          sops = lib.mkIf osConfig.service.sops.enable {
            secrets = {
              "forge_auth" = { };
              "pr_auth" = { };
              "github_token" = { };
            };

            templates."authinfo" = {
              path = "${config.home.homeDirectory}/.authinfo";
              content = ''
                ${config.sops.placeholder."forge_auth"}
                ${config.sops.placeholder."pr_auth"}
              '';
            };

            templates."gh-hosts.yml" = {
              path = "${config.xdg.configHome}/gh/hosts.yml";
              content = ''
                github.com:
                  users:
                    Gako358:
                      oauth_token: ${config.sops.placeholder."github_token"}
                  git_protocol: https
                  oauth_token: ${config.sops.placeholder."github_token"}
                  user: Gako358
              '';
            };
          };
        })
        (lib.mkIf config.programs.merrinx-emacs.enable {
          programs.merrinx-emacs.eca.nixMcp = {
            enable = true;
            roots = [
              "${config.home.homeDirectory}/Projects/emacs-flake"
              "${config.home.homeDirectory}/Sources/dotfiles"
            ];
          };
          programs.merrinx-emacs.eca.ghMcp = {
            enable = true;
            owners = [
              "Gako358"
              "Kvalitetsregistre-OQR"
              "HNIKT-Tjenesteutvikling-Systemutvikling"
            ];
          };
        })
      ];
    };
}
