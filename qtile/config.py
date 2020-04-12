"""
My config files for qtile
>> qtile docs can be found @ http://qtile.readthedocs.io/en/latest/

There are probably some more good hooks to make use of in here:
    http://qtile.readthedocs.io/en/latest/manual/ref/hooks.html
"""
import os

# qtile internals
from libqtile import bar, widget
from libqtile.config import Screen, hook

# Settings/helpers
from settings import COLS, FONT_PARAMS, WITH_SYS_TRAY
from helpers import run_script

# Import the parts of my config defined in other files
from layouts import layouts, floating_layout    # NOQA
from bindings import keys, mouse                # NOQA
from groups import groups                       # NOQA
from widgets import ShellScript


# ----------------------------------------------------------------------------
# Hooks
@hook.subscribe.startup_complete
def autostart():
    """
    My startup script has a sleep in it as some of the commands depend on
    state from the rest of the init. This will cause start/restart of qtile
    to hang slightly as the sleep runs.
    """
    os.environ.setdefault('RUNNING_QTILE', 'True')
    run_script("autostart.sh")


@hook.subscribe.screen_change
def restart_on_randr(qtile, ev):
    """
    Restart and reload config when screens are changed so that we correctly
    init any new screens and correctly remove any old screens that we no
    longer need.

    There is an annoying side effect of removing a second monitor that results
    in windows being 'stuck' on the now invisible desktop...
    """
    qtile.cmd_restart()


@hook.subscribe.setgroup
def remove_scratchpad_on_group_change():
    """
    If we were showing windows from the scratchpad when we move to a new
    group, we hide them again automatically.
    """
    previous_group = hook.qtile.currentScreen.previous_group
    if not previous_group:
        # No windows to hide
        return

    for w in list(previous_group.windows):
        if w.on_scratchpad:
            w.togroup('scratchpad')


@hook.subscribe.client_new
def init_scratchpad_on_new(window):
    """
    When a new window gets created, set the `on_scratchpad` property to false
    so that it is there for us to filter on later when we use scratchpad
    actions.
    """
    window.on_scratchpad = False


# ----------------------------------------------------------------------------
def make_screen(systray=False):
    """Defined as a function so that I can duplicate this on other monitors"""
    def _separator():
        # return widget.Sep(linewidth=2, foreground=COLS["dark_3"])
        return widget.Sep(linewidth=2, foreground=COLS["deus_1"])

    blocks = [
        # Marker for the start of the groups to give a nice bg: ◢■■■■■■■◤
        widget.TextBox(
            font="Arial", foreground=COLS["dark_4"],
            # font="Arial", foreground=COLS["deus_3"],
            text="◢", fontsize=50, padding=-1
        ),
        widget.GroupBox(
            other_current_screen_border=COLS["orange_0"],
            this_current_screen_border=COLS["blue_0"],
            # this_current_screen_border=COLS["deus_2"],
            other_screen_border=COLS["orange_0"],
            this_screen_border=COLS["blue_0"],
            # this_screen_border=COLS["deus_2"],
            highlight_color=COLS["blue_0"],
            # highlight_color=COLS["deus_2"],
            urgent_border=COLS["red_1"],
            background=COLS["dark_4"],
            # background=COLS["deus_3"],
            highlight_method="line",
            inactive=COLS["dark_2"],
            active=COLS["light_2"],
            disable_drag=True,
            borderwidth=2,
            **FONT_PARAMS,
        ),
        # Marker for the end of the groups to give a nice bg: ◢■■■■■■■◤
        widget.TextBox(
            font="Arial", foreground=COLS["dark_4"],
            # font="Arial", foreground=COLS["deus_3"],
            text="◤ ", fontsize=50, padding=-5
        ),
        # Show the title for the focused window
        widget.WindowName(**FONT_PARAMS),
        # Allow for quick command execution
        widget.Prompt(
            cursor_color=COLS["light_3"],
            # ignore_dups_history=True,
            bell_style="visual",
            prompt="λ : ",
            **FONT_PARAMS
        ),
        _separator(),
        # Resource usage graphs
        widget.CPUGraph(
            border_color=COLS["yellow_1"],
            graph_color=COLS["yellow_1"],
            border_width=1,
            line_width=1,
            type="line",
            width=50,
            **FONT_PARAMS
        ),
        widget.MemoryGraph(
            border_color=COLS["blue_2"],
            graph_color=COLS["blue_2"],
            border_width=1,
            line_width=1,
            type="line",
            width=50,
            **FONT_PARAMS
        ),
        widget.NetGraph(
            border_color=COLS["green_1"],
            graph_color=COLS["green_1"],
            border_width=1,
            line_width=1,
            type="line",
            width=50,
            **FONT_PARAMS
        ),
        # IP information
        ShellScript(
            fname="ipadr.sh",
            update_interval=10,
            markup=True,
            padding=1,
            **FONT_PARAMS
        ),
        # Available apt upgrades
        ShellScript(
            fname="aptupgrades.sh",
            update_interval=600,
            markup=True,
            padding=1,
            **FONT_PARAMS
        ),
        # Current battery level
        ShellScript(
            fname="battery.sh",
            update_interval=60,
            markup=True,
            padding=1,
            **FONT_PARAMS
        ),
        # Wifi strength
        ShellScript(
            fname="wifi-signal.sh",
            update_interval=60,
            markup=True,
            padding=1,
            **FONT_PARAMS
        ),
        # Volume % : scroll mouse wheel to change volume
        widget.TextBox("", **FONT_PARAMS),
        widget.Volume(**FONT_PARAMS),
        _separator(),
        # Current time
        widget.Clock(
            format="%Y-%m-%d %a %I:%M %p",
            **FONT_PARAMS
        ),
        # Keyboard layout
        widget.KeyboardLayout(
            configured_keyboards=['us', 'gb'],
            **FONT_PARAMS
        ),
        # Visual indicator of the current layout for this workspace.
        widget.CurrentLayoutIcon(
            custom_icon_paths=[os.path.expanduser("~/.config/qtile/icons")],
            **FONT_PARAMS
        ),
    ]

    if systray:
        # Add in the systray and additional separator
        blocks.insert(-1, widget.Systray())
        blocks.insert(-1, _separator())

    # return Screen(top=bar.Bar(blocks, 25, background=COLS["deus_1"]))
    return Screen(top=bar.Bar(blocks, 25, background=COLS["dark_2"]))


# XXX : When I run qtile inside of mate, I don"t actually want a qtile systray
#       as mate handles that. (Plus, if it _is_ enabled then the mate and
#       qtile trays both crap out...)
screens = [make_screen(systray=WITH_SYS_TRAY)]

# ----------------------------------------------------------------------------
# .: Assorted additional config :.
focus_on_window_activation = "smart"
dgroups_key_binder = None
follow_mouse_focus = True
bring_front_click = False
auto_fullscreen = True
dgroups_app_rules = []
cursor_warp = True
# main = None

# XXX :: Horrible hack needed to make grumpy java apps work correctly.
#        (This is from the default config)
wmname = "LG3D"


# ----------------------------------------------------------------------------
def main(qtile):
    """Optional entry point for the config"""
    # Make sure that we have a screen / bar for each monitor that is attached
    while len(screens) < len(qtile.conn.pseudoscreens):
        screens.append(make_screen())
