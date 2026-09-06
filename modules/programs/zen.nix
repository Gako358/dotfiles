_: {
  flake.homeModules.programs-zen =
    {
      osConfig,
      config,
      inputs,
      pkgs,
      lib,
      ...
    }:
    let
      zen =
        pkgs.wrapFirefox
          inputs.zen-browser.packages."${pkgs.stdenv.hostPlatform.system}".zen-browser-unwrapped
          {
            pname = "zen-browser";
            extraPolicies = {
              DisableAppUpdate = true;
              DisableTelemetry = true;
              DisablePocket = true;
              ExtensionSettings = {
                "78272b6fa58f4a1abaac99321d503a20@proton.me" = {
                  installation_mode = "force_installed";
                  install_url = "https://addons.mozilla.org/firefox/downloads/latest/proton-pass/latest.xpi";
                };
              };
            };
          };
    in
    {
      home = lib.mkIf osConfig.environment.desktop.enable {
        packages = [ zen ];
        persistence."/persist" =
          lib.mkIf
            (
              !(
                osConfig.environment.desktop.windowManager == "kde"
                && lib.elem config.home.username osConfig.environment.desktop.kde.persistenceUsers
              )
            )
            {
              directories = [ ".zen" ];
            };
      };
    };
}
