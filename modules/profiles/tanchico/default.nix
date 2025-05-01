{ osConfig
, lib
, ...
}:
{
  imports = [ ../../default.nix ];

  wayland.windowManager.hyprland.extraConfig = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") ''
    monitor=HDMI-A-1,3840x2160,0x0,1
    monitor=,highrr,auto,1

    workspace = 1, monitor:HDMI-A-1
    workspace = 2, monitor:HDMI-A-1
    workspace = 3, monitor:HDMI-A-1
    workspace = 4, monitor:HDMI-A-1
    workspace = 5, monitor:HDMI-A-1
    workspace = 6, monitor:HDMI-A-1
    workspace = 7, monitor:HDMI-A-1
    workspace = 8, monitor:HDMI-A-1
    workspace = 9, monitor:HDMI-A-1
  '';

  # Home modules to load
  program.hyprlock.defaultMonitor = "HDMI-A-1";

  service.hypridle = {
    timeout = 3600;
    suspend = 600;
  };
}
