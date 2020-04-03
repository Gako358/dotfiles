import os
import re
import socket
import subprocess
from libqtile.config import Key, Screen, Group, Drag, Click, Rule
from libqtile.command import lazy, Client
from libqtile import layout, bar, widget, hook, extension
from libqtile.widget import Spacer

from typing import List  # noqa: F401

mod = "mod4"

keys = [

    Key([mod], "F1", lazy.spawn('firefox')),
    Key([mod], "F4", lazy.spawn('lyx')),
    Key([mod], "F5", lazy.spawn('gimp')),
    Key([mod], "F6", lazy.spawn('discord')),
    Key([mod], "F7", lazy.spawn('thunderbird')),
    Key([mod], "r", lazy.spawn("urxvt -e ranger")),
    Key([mod], "Return", lazy.spawn("urxvt")),

    Key([mod], "q", lazy.window.kill()),
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod], "w", lazy.spawncmd()),

    # Super + Shift 
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_up()),
    # Super + Ctrl
    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),

    # Increase / Decrease Brightness
    Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight -inc 10")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -dec 10")),

    # Increase / Decrease Volume
    Key([], "XF86AudioMute", lazy.spawn("amixer -q set Master toggle")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer -q set Master 5%-")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -q set Master 5%+")),

    # Change Focus
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),

    # Resize UP, DOWN, LEFT, RIGHT
    Key([mod, "control"], "l",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        ),

    Key([mod, "control"], "h",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        ),

    Key([mod, "control"], "k",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        ),

    Key([mod, "control"], "j",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        ),

    Key([mod], "Tab", lazy.next_layout()),
    Key([mod], "space", lazy.window.toggle_floating()),
]

##### GROUPS #####
group_names = [(" Web", {'layout': 'monadtall'}),
               (" Dev", {'layout': 'monadtall'}),
               (" Sys", {'layout': 'monadtall'}),
               (" Doc", {'layout': 'monadtall'}),
               (" Vbox", {'layout': 'monadtall'}),
               (" Chat", {'layout': 'monadtall'}),
               (" Mail", {'layout': 'monadtall'})]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))        # Switch to another group
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name))) # Send current window to another group

##### DEFAULT THEME SETTINGS FOR LAYOUTS #####
layout_theme = {"border_width": 3,
                "margin": 37,
                "border_focus": "3daee9",
                "border_normal": "1D2330"
                }
layouts = [
    layout.Stack(num_stacks=2),
    layout.MonadTall(**layout_theme),
    layout.Bsp(**layout_theme),
    layout.Max(**layout_theme),
    layout.Floating(**layout_theme)
]

def init_colors():
    return [["#2F343F", "#2F343F"], # color 0
            ["#2F343F", "#2F343F"], # color 1
            ["#c0c5ce", "#c0c5ce"], # color 2
            ["#720000", "#720000"], # color 3
            ["#3384d0", "#3384d0"], # color 4
            ["#f3f4f5", "#f3f4f5"], # color 5
            ["#cd1f3f", "#cd1f3f"], # color 6
            ["#773d8e", "#773d8e"], # color 7 #1a2f56 #62FF00
            ["#6790eb", "#6790eb"], # color 8
            ["#a9a9a9", "#a9a9a9"], # color 9
            ["#16a085", "#16a085"], # color 10 #Cyan
            ["#3daee9", "#3daee9"], # color 11 #Blue
            ["#fba922", "#fba922"], # color 12 #Orange
            ["#3971ed", "#3971ed"], # color 13 #Dark Blue
            ["#cc342b", "#cc342b"]] # color 14 #Red

colors = init_colors()

widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=2,
    background=colors[1]
)

extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.TextBox(
                    font="Arial", foreground= colors[2],
                    text="◢", fontsize=73, padding=-11
                ),
                widget.GroupBox(
                    font="FontAwesome",
                    fontsize = 16,
                    margin_y = -1,
                    margin_x = 0,
                    padding_y = 6,
                    padding_x = 5,
                    borderwidth = 0,
                    active = colors[11],
                    inactive = colors[1],
                    rounded = False,
                    highlight_method = "text",
                    this_current_screen_border = colors[14],
                    foreground = colors[1],
                    background = colors[2]
                ),
                widget.TextBox(
                    font="Arial", foreground= colors[2],
                    text="◤", fontsize=73, padding=-11
                ),
                widget.CurrentLayout(
                    font = "TerminessTTF Nerd Font Medium",
                    fontsize = 13,
                    foreground = colors[11],
                    background = colors[1],
                ),
                widget.CurrentLayoutIcon(
                    font = "TerminessTTF Nerd Font Medium",
                    fontsize = 13,
                    scale = .70,
                    background = colors[2],
                ),
                widget.Sep(
                    linewidth = 1,
                    padding = 10,
                    foreground = colors[2],
                    background = colors[1],
                ),
                widget.Prompt(
                    font = "TerminessTTF Nerd Font Medium",
                    fontsize = 15,
                    foreground = colors[5],
                    background = colors[1],
                ),
                widget.WindowName(
                    font = "TerminessTTF Nerd Font Medium",
                    fontsize = 15,
                    foreground = colors[5],
                    background = colors[1],
                ),
                widget.TextBox(
                    font = "Arial", foreground = colors[2],
                    text = "◢", fontsize=73, padding = -11,
                ),
                widget.TextBox(
                    font = "Arial", foreground = colors[1],
                    text = "⟳", fontsize=28, padding = 0,
                    background = colors[2],
                ),
                widget.Pacman(
                    foreground = colors[1],
                    background = colors[2],
                    padding = 5,
                ),
                widget.BatteryIcon(
                    foreground = colors[1],
                    background = colors[2],
                    padding = 0,
                ),
                widget.Battery(
                    foreground = colors[1],
                    background = colors[2],
                    padding = 5,
                    discharge_char = '',
                    format = '{char} {percent:2.0%}',
                    show_short_text = True,
                ),
                widget.TextBox(
                    font = "Arial", foreground = colors[1],
                    text = "", fontsize = 28, padding = 0,
                    background = colors[2],
                ),
                widget.Memory(
                    foreground = colors[1],
                    background = colors[2],
                    padding = 5,
                ),
                widget.TextBox(
                    font = "Arial", foreground = colors[1],
                    text = "↯", fontsize = 28, padding = 0,
                    background = colors[2],
                ),
                widget.Net(
                    interface = "wlp2s0",
                    foreground = colors[1],
                    background = colors[2],
                    padding = 5,
                ),
                widget.TextBox( 
                    font = "Arial", foreground = colors[1],
                    text = "♫", fontsize = 28, padding = 0,
                    background = colors[2],
                ),
                widget.Volume(
                    foreground = colors[1],
                    background = colors[2],
                    padding = 5,
                ),
                widget.TextBox(
                    font = "Arial", foreground = colors[3],
                    text = "", fontsize=28, padding = 0,
                    background = colors[2],
                ),
                widget.Clock(
                    foreground = colors[1],
                    background = colors[2],
                    format="%d-%m-%Y %H: %M",
                    font = "TerminessTTF Nerd Font Medium",
                    fonsize = 19,
                ),
                widget.TextBox(
                    font="Arial", foreground = colors[2],
                    text ="◤", fontsize = 73, padding = -11,
                ),
                widget.LaunchBar(progs=[('gimp', 'gimp', 'start gimp')]),
                widget.Systray(
                    background = colors[1],
                    foreground = colors[1],
                    icon_size = 25,
                    pading = 5,
                ),
            ],
            28,
            opacity=0.91,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None

@hook.subscribe.startup_once
def start_once():
    subprocess.call('/home/merrinx/.config/qtile/scripts/autostart.sh')

follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
