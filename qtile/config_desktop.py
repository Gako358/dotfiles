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
    Key([mod], "F2", lazy.spawn('thunderbird')),
    Key([mod], "F3", lazy.spawn('discord')),
    Key([mod], "F4", lazy.spawn('lyx')),
    Key([mod], "F5", lazy.spawn('gimp')),
    Key([mod], "F6", lazy.spawn('steam')),
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

    Key([mod, "control"], "x", lazy.spawn("slock")),

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

    Key([mod], "z", 
        lazy.layout.down()),

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

    Key([mod], 's', lazy.to_screen(0)),
    Key([mod], 'd', lazy.to_screen(1)),
    Key([mod], 'a', lazy.to_screen(2)),

    Key([mod], 'm', lazy.layout.maximize()),
    Key([mod], 'n', lazy.layout.normalize()),

    Key([mod], "Tab", lazy.next_layout()),
    Key([mod], "space", lazy.window.toggle_floating()),
]


##### GROUPS #####

group_names = [(" Dev", {'layout': 'monadtall'}),
               (" Web", {'layout': 'stack'}),
               (" Sys", {'layout': 'verticaltile'}),
               (" Chat", {'layout': 'verticaltile'}),
               (" Doc", {'layout': 'monadtall'}),
               (" Steam", {'layout': 'stack'}),
               (" Vbox", {'layout': 'stack'})]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))        # Switch to another group
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name))) # Send current window to another group

##### DEFAULT THEME SETTINGS FOR LAYOUTS #####
layout_theme = {"border_width": 3,
                "margin": 19,
                "border_focus": "569cd6",
                "border_normal": "9cdcfe"
                }
layouts = [
    layout.Stack(num_stacks=1,margin=19),
    layout.MonadTall(**layout_theme),
    layout.Floating(**layout_theme),
    layout.VerticalTile(**layout_theme)
]

def init_colors():
    return [["#1d2021", "#1d2021"], # color 0 Black
            ["#ebdbb2", "#ebdbb2"], # color 1 Text
            ["#f92672", "#f92672"], # color 2 Red
            ["#a6e22e", "#a6e22e"], # color 3 Green
            ["#f4bf75", "#f4bf75"], # color 4 Yellow
            ["#66d9ef", "#66d9ef"], # color 5 Blue
            ["#ae81ff", "#ae81ff"], # color 6 Magenta
            ["#2aa198", "#2aa198"], # color 7 Cyan
            ["#f8f8f2", "#f8f8f2"]] # color 8 White

colors = init_colors()

widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=2,
    background=colors[0]
)

extension_defaults = widget_defaults.copy()

def init_widgets_list():
    widgets_list = [
        widget.TextBox(
            font="Arial", foreground= colors[5],
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
            active = colors[2],
            inactive = colors[8],
            rounded = False,
            highlight_method = "text",
            this_current_screen_border = colors[0],
            foreground = colors[8],
            background = colors[5]
        ),
        widget.TextBox(
            font="Arial", foreground= colors[5],
            text="◤", fontsize=73, padding=-11
        ),
        widget.CurrentLayout(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 13,
            foreground = colors[4],
            background = colors[0],
        ),
        widget.CurrentLayoutIcon(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 13,
            scale = .70,
            background = colors[3],
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[8],
            background = colors[0],
        ),
        widget.Prompt(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 15,
            foreground = colors[4],
            background = colors[0],
        ),
        widget.WindowName(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 15,
            foreground = colors[4],
            background = colors[0],
        ),
        widget.LaunchBar(progs=[
            ('Lock', 'slock', 'Screen Lock')],
            default_icon = '/home/merrinx/Pictures/Buttons/lock.png',
            padding = 0,
            background = colors[0],
        ),
        widget.LaunchBar(progs=[
            ('Sleep', 'systemctl suspend', 'Sleep')],
            default_icon = '/home/merrinx/Pictures/Buttons/sleep.png',
            padding = 0,
            background = colors[0],
        ),
        widget.LaunchBar(progs=[
            ('Reboot', 'systemctl reboot', 'Reboot')],
            default_icon = '/home/merrinx/Pictures/Buttons/logout.png',
            padding = 0,
            background = colors[0],
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[5],
            text = "◢", fontsize=73, padding = -11,
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[7],
            text = "⟳", fontsize=28, padding = 0,
            background = colors[5],
        ),
        widget.Pacman(
            foreground = colors[8],
            background = colors[5],
            padding = 5,
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[0],
            text = "", fontsize = 28, padding = 0,
            background = colors[5],
        ),
        widget.Memory(
            foreground = colors[8],
            background = colors[5],
            padding = 5,
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[4],
            text = "↯", fontsize = 28, padding = 0,
            background = colors[5],
        ),
        widget.Net(
            interface = "wlp5s0",
            foreground = colors[8],
            background = colors[5],
            padding = 5,
        ),
        widget.TextBox( 
            font = "Arial", foreground = colors[6],
            text = "♫", fontsize = 28, padding = 0,
            background = colors[5],
        ),
        widget.Volume(
            foreground = colors[8],
            background = colors[5],
            padding = 5,
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[2],
            text = "", fontsize=28, padding = 0,
            background = colors[5],
        ),
        widget.Clock(
            foreground = colors[8],
            background = colors[5],
            format="%d-%m-%Y %H: %M",
            font = "TerminessTTF Nerd Font Medium",
            fonsize = 19,
        ),
        widget.TextBox(
            font="Arial", foreground = colors[5],
            text ="◤", fontsize = 73, padding = -11,
        ),
        widget.KeyboardLayout(
            configured_keyboards = ['us', 'no'],
        ),
        widget.Systray(
            icon_size = 25,
            pading = 5,
        ),
    ]
    return widgets_list

def init_widgets_list_slave():
    widgets_list = [
        widget.TextBox(
            font="Arial", foreground= colors[5],
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
            active = colors[2],
            inactive = colors[8],
            rounded = False,
            highlight_method = "text",
            this_current_screen_border = colors[0],
            foreground = colors[8],
            background = colors[5]
        ),
        widget.TextBox(
            font="Arial", foreground= colors[5],
            text="◤", fontsize=73, padding=-11
        ),
        widget.CurrentLayout(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 13,
            foreground = colors[4],
            background = colors[0],
        ),
        widget.CurrentLayoutIcon(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 13,
            scale = .70,
            background = colors[3],
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[8],
            background = colors[0],
        ),
        widget.Prompt(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 15,
            foreground = colors[4],
            background = colors[0],
        ),
        widget.WindowName(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 15,
            foreground = colors[4],
            background = colors[0],
        ),
    ]
    return widgets_list

def init_widgets_screen1():
    widgets_screen1 = init_widgets_list()
    return widgets_screen1

def init_widgets_screen2():
    widgets_screen2 = init_widgets_list_slave()
    return widgets_screen2

def init_screens():
    return [Screen(top=bar.Bar(widgets=init_widgets_screen1(), opacity=0.99, size=28)),
            Screen(top=bar.Bar(widgets=init_widgets_screen2(), opacity=0.99, size=28)),
            Screen(top=bar.Bar(widgets=init_widgets_screen2(), opacity=0.99, size=28))]

if __name__ in ["config", "__main__"]:
    screens = init_screens()
    widgets_list = init_widgets_list()
    widgets_screen1 = init_widgets_screen1()
    widgets_screen2 = init_widgets_screen2()

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
auto_fullscreen = False
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
