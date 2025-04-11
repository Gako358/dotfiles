{
  services.btrbk = {
    instances."btrbak" = {
      onCalendar = "*-*-* *:00:00";
      settings = {
        timestamp_format = "long";
        snapshot_preserve_min = "2d";
        preserve_day_of_week = "sunday";
        preserve_hour_of_day = "0";
        target_preserve = "48h 10d 4w 12m 10y";
        volume."/persist" = {
          snapshot_create = "always";
          subvolume = ".";
          snapshot_dir = "snapshots/persist";
        };
        volume."/btrfs_tmp" = {
          snapshot_create = "no";
          subvolume = "root";
          snapshot_dir = "/persist/snapshots/root";
        };
      };
    };
  };
}
