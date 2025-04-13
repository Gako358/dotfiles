{ specialArgs, ... }: {
  services.btrbk = {
    instances."btrbak" = {
      onCalendar = "*-*-* *:00:00";
      settings = {
        timestamp_format = "long";
        snapshot_preserve_min = "2d";
        preserve_day_of_week = "sunday";
        preserve_hour_of_day = "0";
        target_preserve = "48h 7d 4w";
        volume."/persist" = {
          snapshot_create = "always";
          subvolume = ".";
          snapshot_dir = "snapshots/persist";
        };
        volume."/nix" =
          if specialArgs.master then {
            snapshot_create = "always";
            subvolume = ".";
            snapshot_dir = "/persist/snapshots/nix";
          } else { };
      };
    };
  };
}
