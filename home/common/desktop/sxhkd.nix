{ config
, lib
, pkgs
, ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in
{
  config = mkIf (cfg.environment == "bspwm") {
    services.sxhkd = {
      enable = true;
      keybindings = {
        # terminal emulator
        "super + Return" = "st";

        "super + w" = "pcmanfm";

        # show run launcher
        "super + c" = "rofi -show run";

        # show power menu
        "super + shift + q" = "rofi -show power-menu -modi power-menu:rofi-power-menu";

        # Launch music daemon
        "super + shift + n" = "st -e ncspot";

        # chat
        "super + shift + c" = "st -e 'weechat' -e weechat";

        # screenshot
        "super + shift + alt + p" = "scrot -d3 /home/merrinx/Pictures/Screenshots/";

        # screensaver
        "super + shift + l" = "slock";

        # suspend
        "super + shift + p" = "systemctl suspend";

        # set opacity
        "super + ctrl + a" = "picom-trans -c -10";

        "super + ctrl + shift + a" = "picom-trans -c +100";

        # norwegian keyboard
        "super + shift + ctrl + n" = "setxkbmap -layout no";

        # english keyboard
        "super + shift + ctrl + u" = "setxkbmap -layout us";

        # make sxhkd reload its configuration files:
        "super + Escape" = "pkill -USR1 -x sxhkd";

        # volume ug
        "XF86AudioRaiseVolume" = "amixer -c 1 sset Master 5%+";

        # volume down
        "XF86AudioLowerVolume" = "amixer -c 1 sset Master 5%-";

        # volume mute
        "XF86AudioMute" = "amixer -q sset Master toggle";

        # quit/restart bspwm
        "super + alt + {q,r}" = "bspc {quit,wm -r}";

        # close and kill
        "super + {_,shift + }q" = "bspc node -{c,k}";

        # alternate between the tiled and monocle layout
        "super + m" = "bspc desktop -l next";

        # send the newest marked node to the newest preselected node
        "super + y" = "bspc node newest.marked.local -n newest.!automatic.local";

        # swap the current node and the biggest window
        "super + g" = "bspc node -s biggest.window";

        # set the window stat
        "super + {t,shift + t,s,f}" = "bspc node -t {tiled,pseudo_tiled,floating,fullscreen}";

        # set the node flags
        "super + ctrl + {m,x,y,z}" = "bspc node -g {marked,locked,sticky,private}";

        # focus the node in the given direction
        "super + {_,shift + }{h,j,k,l}" = "bspc node -{f,s} {west,south,north,east}";

        # focus the node for the given path jump
        "super + {p,b,comma,period}" = "bspc node -f @{parent,brother,first,second}";

        # focus the next/previous window in the current desktop
        "super + {_,shift + }c" = "bspc node -f {next,prev}.local.!hidden.window";

        # focus the next/previous desktop in the current monitor
        "super + bracket{left,right}" = "bspc desktop -f {prev,next}.local";

        # focus the last node/desktop
        "super + {grave,Tab}" = "bspc {node,desktop} -f last";

        # focus or send to the given desktop
        "super + {_,shift + }{1-9,0}" = "bspc {desktop -f,node -d} '^{1-9,10}'";

        # preselect the direction
        "super + ctrl + {h,j,k,l}" = "bspc node -p {west,south,north,east}";

        # preselect the ratio
        "super + ctrl + {1-9}" = "bspc node -o 0.{1-9}";

        # cancel the preselection for the focused node
        "super + ctrl + space" = "bspc node -p cancel";

        # cancel the preselection for the focused desktop
        "super + ctrl + shift + space" = "bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel";

        # expand a window by moving one of its side outward
        "super + alt + {h,j,k,l}" = "bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}";

        # contract a window by moving one of its side inward
        "super + alt + shift + {h,j,k,l}" = "bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}";

        # move a floating window
        "super + {Left,Down,Up,Right}" = "bspc node -v {-20 0,0 20,0 -20,20 0}";

        # Rotate tree
        "super + shift + {a,d}" = "bspc node @/ -C {forward,backward}";
      };
    };
  };
}
