_: {
  flake.nixosModules.programs-corectrl =
    {
      config,
      lib,
      ...
    }:
    {
      config = lib.mkIf config.environment.gaming.enable {
        programs.corectrl.enable = true;

        hardware.amdgpu.overdrive.enable = true;

        users.users.merrinx.extraGroups = [ "corectrl" ];

        environment.persistence."/persist" = {
          users.merrinx = {
            directories = [
              ".config/CoreCtrl"
            ];
          };
        };
      };
    };
}
