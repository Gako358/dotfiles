{specialArgs, ...}:
if specialArgs.hidpi
then {
  wayland.windowManager.hyprland.extraConfig = ''
    monitor=DP-2,3840x2160,0x0,1,bitdepth,10
    monitor=,highrr,auto,1
  '';
}
else {
  wayland.windowManager.hyprland.extraConfig = ''
    monitor=eDP-1,1920x1080,0x0,1,bitdepth,10
    monitor=DP-8,2560x1440,1920x0,1,bitdepth,10
    monitor=,highrr,auto,1
  '';
}
