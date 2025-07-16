{
  imports = [ ../../default.nix ];

  # Home modules to load
  program.hyprlock.defaultMonitor = "virtual-1";

  service.hypridle = {
    timeout = 3600;
    suspend = 600;
  };
}
