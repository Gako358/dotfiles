{
  services.btrbk = {
    instances."btrbak" = {
      onCalendar = "*-*-* 06,14,22:00:00";
      settings = {
        timestamp_format = "long";
        snapshot_preserve_min = "3d";
        snapshot_preserve = "7d 4w 3m";
        preserve_day_of_week = "sunday";
        preserve_hour_of_day = "6";
        volume."/persist" = {
          snapshot_create = "always";
          subvolume = ".";
          snapshot_dir = "snapshots/persist";
        };
      };
    };
  };
}
