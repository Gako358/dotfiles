{specialArgs, ...}:
if specialArgs.hidpi
then {
  wayland.windowManager.hyprland.extraConfig = ''
    monitor=DP-3,2560x1440,0x0,1,bitdepth,10
    monitor=DP-2,1920x1080,2560x1080,1,bitdepth,10
    monitor=HDMI-A-1,1920x1080,2560x0,1,bitdepth,10
    monitor=,highrr,auto,1
  '';
}
else {
  wayland.windowManager.hyprland.extraConfig = ''
    monitor=eDP-1,1920x1080,2560x0,1,bitdepth,10
    monitor=DP-8,2560x1440,4480x0,1,bitdepth,10
    monitor=DP-9,2560x1440,0x0,1,bitdepth,10
    monitor=,highrr,auto,1
  '';
}
