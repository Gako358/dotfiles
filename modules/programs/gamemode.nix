_: {
  flake.nixosModules.programs-gamemode =
    {
      config,
      lib,
      ...
    }:
    {
      config = lib.mkIf config.environment.gaming.enable {
        programs.gamemode = {
          enable = true;
          enableRenice = true;
          settings = {
            general = {
              renice = 10;
              inhibit_screensaver = 1;
              desiredgov = "performance";
              defaultgov = "powersave";
            };

            cpu = {
              park_cores = "no";
              pin_cores = "no";
            };

            gpu = {
              apply_gpu_optimisations = "accept-responsibility";
              gpu_device = 0;
              amd_performance_level = "high";
            };
          };
        };

        users.users.merrinx.extraGroups = [ "gamemode" ];
      };
    };
}
