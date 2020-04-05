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

    # Increase / Decrease Volume
    Key([], "XF86AudioMute", lazy.spawn("amixer -q set Master toggle")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer -q set Master 5%-")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -q set Master 5%+")),

    # Change Focus
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    
    Key([mod], "z", lazy.layout.down()),
    Key([mod], "x", lazy.layout.down()),

    # Change Focus to selected screen
    Key([mod], "a", lazy.to_screen(0)),
    Key([mod], "d", lazy.to_screen(1)),

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
group_names = [(" Web", {'layout': 'floating'}),
               (" Sys", {'layout': 'monadtall'}),
               (" Chat", {'layout': 'stack'}),
               (" Dev", {'layout': 'monadtall'}),
               (" Doc", {'layout': 'monadtall'}),
               (" Steam", {'layout': 'stack'}),
               (" Vbox", {'layout': 'stack'})]

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
    layout.Stack(num_stacks=1,margin=37),
    layout.MonadTall(**layout_theme),
    layout.Floating(**layout_theme)
]

def init_colors():
    return [["#2F343F", "#2F343F"], # color 0 Dark Grey
            ["#2F343F", "#2F343F"], # color 1 Dark Grey
            ["#c0c5ce", "#c0c5ce"], # color 2 Cream White
            ["#272822", "#272822"], # color 3 Black
            ["#75715e", "#75715e"], # color 4 Brown
            ["#c30046", "#c30046"], # color 5 Red
            ["#a4f600", "#a4f600"], # color 6 Green
            ["#f4bf75", "#f4bf75"], # color 7 Yellow
            ["#20b4ea", "#20b4ea"], # color 8 Blue
            ["#ae81ff", "#ae81ff"], # color 9 Magneta
            ["#a1efe4", "#a1efe4"], # color 10 Cyan
            ["#f8f8f2", "#f8f8f2"], # color 11 White
            ["#c36a05", "#c36a05"]] # color 12 Orange

colors = init_colors()

widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=2,
    background=colors[3]
)

extension_defaults = widget_defaults.copy()

def init_widgets_list_1():
    widgets_list = [
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
            active = colors[8],
            inactive = colors[1],
            rounded = False,
            highlight_method = "text",
            this_current_screen_border = colors[5],
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
            foreground = colors[8],
            background = colors[3],
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
            background = colors[3],
        ),
        widget.Prompt(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 15,
            foreground = colors[4],
            background = colors[3],
        ),
        widget.WindowName(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 15,
            foreground = colors[8],
            background = colors[3],
        ),
        widget.LaunchBar(progs=[
            ('Lock', 'slock', 'Screen Lock')],
            default_icon = '/home/merrinx/Pictures/Buttons/lock.png',
            padding = 0,
            background = colors[3],
        ),
        widget.LaunchBar(progs=[
            ('Sleep', 'systemctl suspend', 'Sleep')],
            default_icon = '/home/merrinx/Pictures/Buttons/sleep.png',
            padding = 0,
            background = colors[3],
        ),
        widget.LaunchBar(progs=[
            ('Logout', 'qshell:self.qtile.cmd_shutdown()', 'Logout')],
            default_icon = '/home/merrinx/Pictures/Buttons/logout.png',
            padding = 0,
            background = colors[3],
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[2],
            text = "◢", fontsize=73, padding = -11,
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[12],
            text = "⟳", fontsize=28, padding = 0,
            background = colors[2],
        ),
        widget.Pacman(
            foreground = colors[1],
            background = colors[2],
            padding = 5,
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[12],
            text = "", fontsize = 28, padding = 0,
            background = colors[2],
        ),
        widget.Memory(
            foreground = colors[1],
            background = colors[2],
            padding = 5,
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[12],
            text = "↯", fontsize = 28, padding = 0,
            background = colors[2],
        ),
        widget.Net(
            interface = "wlp5s0",
            foreground = colors[1],
            background = colors[2],
            padding = 5,
        ),
        widget.TextBox( 
            font = "Arial", foreground = colors[12],
            text = "♫", fontsize = 28, padding = 0,
            background = colors[2],
        ),
        widget.Volume(
            foreground = colors[1],
            background = colors[2],
            padding = 5,
        ),
        widget.TextBox(
            font = "Arial", foreground = colors[5],
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
        widget.KeyboardLayout(
            configured_keyboards = ['us', 'no'],
            background = colors[3],
            foreground = colors[11],
        ),
        widget.Systray(
            background = colors[3],
            foreground = colors[1],
            icon_size = 25,
            pading = 5,
        ),
    ]
    return widgets_list

def init_widgets_list_2():
    widgets_list = [
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
            active = colors[8],
            inactive = colors[1],
            rounded = False,
            highlight_method = "text",
            this_current_screen_border = colors[5],
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
            foreground = colors[8],
            background = colors[3],
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
            background = colors[3],
        ),
        widget.Prompt(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 15,
            foreground = colors[4],
            background = colors[3],
        ),
        widget.WindowName(
            font = "TerminessTTF Nerd Font Medium",
            fontsize = 15,
            foreground = colors[8],
            background = colors[3],
        ),
    ]
    return widgets_list

def init_widgets_screen1():
    widgets_screen1 = init_widgets_list_1()
    return widgets_screen1

def init_widgets_screen2():
    widgets_screen2 = init_widgets_list_2()
    return widgets_screen2

def init_screens():
    return [Screen(top=bar.Bar(widgets=init_widgets_screen1(), opacity=0.91, size=28)),
            Screen(top=bar.Bar(widgets=init_widgets_screen2(), opacity=0.91, size=28))]

if __name__ in ["config", "__main__"]:
    screens = init_screens()
    widgets_list = init_widgets_list_1()
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

##### STARTUP APPLICATIONS #####
@hook.subscribe.startup_once
def start_once():
    subprocess.call('/home/merrinx/.config/qtile/scripts/autostart.sh')

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
