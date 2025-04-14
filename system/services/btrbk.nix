{
  services.btrbk = {
    instances."btrbak" = {
      # Take snapshots three times a day: 6am, 2pm, and 10pm
      onCalendar = "*-*-* 06,14,22:00:00";
      settings = {
        timestamp_format = "long";
        # Retention settings
        snapshot_preserve_min = "3d"; # Keep all snapshots for at least 2 days
        preserve_day_of_week = "sunday"; # Keep Sunday snapshots longer
        preserve_hour_of_day = "6"; # Use the 6am snapshot for weekly retention
        target_preserve = "48h 7d 4w";
        volume."/persist" = {
          snapshot_create = "always";
          subvolume = ".";
          snapshot_dir = "snapshots/persist";
        };
      };
    };
  };
}
