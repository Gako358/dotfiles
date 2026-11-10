_: {
  flake.nixosModules.programs-obs =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    let
      cfg = config.program.obs;
    in
    {
      options.program.obs = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable OBS Studio with keybind-driven recording.";
        };

        recorder = lib.mkOption {
          type = lib.types.package;
          readOnly = true;
          default = pkgs.callPackage ./_record.nix {
            obs = config.programs.obs-studio.finalPackage;
          };
          description = "The obs-record script used by the Hyprland keybind and Quickshell bar.";
        };
      };

      config = lib.mkIf (cfg.enable && config.environment.desktop.enable) {
        programs.obs-studio = {
          enable = true;
          plugins = with pkgs.obs-studio-plugins; [
            obs-pipewire-audio-capture
            wlrobs
          ];
        };
      };
    };

  flake.homeModules.programs-obs =
    {
      osConfig,
      lib,
      ...
    }:
    let
      cfg = osConfig.program.obs;
    in
    {
      config = lib.mkIf (cfg.enable && osConfig.environment.desktop.enable) {
        home = {
          packages = [ cfg.recorder ];
          persistence."/persist/" = {
            directories = [
              ".config/obs-studio"
              "Videos"
            ];
          };
        };
      };
    };
}
