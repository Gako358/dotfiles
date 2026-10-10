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
      # Upstream still sets the pre-rename passthru flags, so wrapFirefox drops
      # ffmpeg from the library path and media playback fails.
      # Drop once https://github.com/youwen5/zen-browser-flake/pull/20 lands.
      zen-unwrapped =
        inputs.zen-browser.packages."${pkgs.stdenv.hostPlatform.system}".zen-browser-unwrapped.overrideAttrs
          (prev: {
            passthru = prev.passthru // {
              withGSSAPI = true;
              withFFmpeg = true;
            };
          });
      zen = pkgs.wrapFirefox zen-unwrapped {
        pname = "zen-browser";
        extraPolicies = {
          DisableAppUpdate = true;
          DisableTelemetry = true;
          DisablePocket = true;
          Preferences = {
            "browser.tabs.unloadOnLowMemory" = {
              Value = true;
              Status = "default";
            };
          };
          ExtensionSettings = {
            "78272b6fa58f4a1abaac99321d503a20@proton.me" = {
              installation_mode = "force_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/proton-pass/latest.xpi";
            };
            "{c2c003ee-bd69-42a2-b0e9-6f34222cb046}" = {
              installation_mode = "force_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/auto-tab-discard/latest.xpi";
            };
          };
          "3rdparty".Extensions."{c2c003ee-bd69-42a2-b0e9-6f34222cb046}".period = 600;
        };
      };
    in
    {
      options.programs.zen.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable Zen Browser.";
      };

      config = lib.mkIf (osConfig.environment.desktop.enable && config.programs.zen.enable) {
        home = {
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
    };
}
