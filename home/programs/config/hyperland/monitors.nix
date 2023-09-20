{specialArgs, ...}: {
  wayland.windowManager.hyprland.extraConfig =
    if specialArgs.hidpi
    then ''
      monitor=HDMI-A-1,2560x1440,0x0,1,bitdepth,10
      monitor=DP-2,2560x1440,2560x0,1,bitdepth,10
      monitor=,highrr,auto,1
    ''
    else ''
      monitor=eDP-1,1920x1080,0x0,1,bitdepth,10
      monitor=DP-7,2560x1440,1920x0,1,bitdepth,10
      monitor=DP-5,2560x1440,4480x0,1,bitdepth,10
      monitor=,highrr,auto,1
    '';
}
