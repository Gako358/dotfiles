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

      config = lib.mkIf (desktop.enable && desktop.develop) {
        programs.merrinx-emacs.enable = true;

        programs.fish.shellAliases = {
          vim = "emacs -nw";
          vi = "emacs -nw";
        };

        sops = lib.mkIf osConfig.service.sops.enable {
          secrets = {
            "forge_auth" = { };
            "pr_auth" = { };
          };

          templates."authinfo" = {
            path = "${config.home.homeDirectory}/.authinfo";
            content = ''
              ${config.sops.placeholder."forge_auth"}
              ${config.sops.placeholder."pr_auth"}
            '';
          };
        };
      };
    };
}
