_: {
  flake.nixosModules.programs-steam =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      config = lib.mkIf config.environment.gaming.enable {
        programs = {
          steam = {
            enable = true;
            extraCompatPackages = [
              pkgs.proton-ge-bin
            ];
            gamescopeSession = {
              enable = true;
              args = [
                "--prefer-output"
                "HDMI-A-1"
                "--output-width"
                "3840"
                "--output-height"
                "2160"
                "--nested-refresh"
                "60"
              ];
            };

            remotePlay.openFirewall = true;
            dedicatedServer.openFirewall = true;
            localNetworkGameTransfers.openFirewall = true;

            protontricks.enable = true;

            extraPackages = [
              pkgs.mangohud
            ];
          };

          gamescope = {
            enable = true;
            capSysNice = true;
          };
        };

        environment.persistence."/persist" = {
          users.merrinx = {
            directories = [
              ".steam"
              ".local/share/Steam"
              ".cache/mesa_shader_cache"
              ".cache/mesa_shader_cache_db"
            ];
          };
        };
      };
    };
}
