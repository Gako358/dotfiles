{ osConfig
, lib
, ...
}:
{
  imports = [ ../../default.nix ];

  wayland.windowManager.hyprland.extraConfig = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") ''
    monitor=virtual-1,2560x1440,0x0,1
    monitor=,highrr,auto,1

    workspace = 1, monitor:virtual-1
    workspace = 2, monitor:virtual-1
    workspace = 3, monitor:virtual-1
    workspace = 4, monitor:virtual-1
    workspace = 5, monitor:virtual-1
    workspace = 6, monitor:virtual-1
    workspace = 7, monitor:virtual-1
    workspace = 8, monitor:virtual-1
    workspace = 9, monitor:virtual-1
  '';

  # Home modules to load
  program.hyprlock.defaultMonitor = "virtual-1";

  service.hypridle = {
    timeout = 3600;
    suspend = 600;
  };
}
