'''
My mouse and key bindings.

String names for non-alpha-numeric keys can be found here:
>>> https://github.com/qtile/qtile/blob/develop/libqtile/xkeysyms.py

It is possible to bind keys to multiple actions (see the swap panes bindings).
When this is done, all actions are sent and the layout/window/group acts on
those that it knows about and ignores those that it doesn't.
- I've used this to group logical behaviour between layouts where they use
  different method names (in the case of moving windows) and to chain
  actions together (move group to screen and follow with focus).

I'm not being 100% consistent but in general:
    M-...  :: qtile / environment commands
    M-S... :: qtile window/group management commands (movement of windows etc)
    M-C... :: program launching
    M-A... :: utility launching

Anything bound to arrow keys is movement based. I'm having problems binding
`M-C={h,j,k,l}` which is preventing me using that for movement. (Though this
may be something to do with my own ez_keys function...!)
'''
import os

from libqtile.config import Click, Drag, EzKey
from libqtile.command import lazy

from settings import MOD, TERMINAL
from helpers import script, notify
from groups import groups


def switch_screens(target_screen):
    '''Send the current group to the other screen.'''
    @lazy.function
    def _inner(qtile):
        current_group = qtile.screens[1 - target_screen].group
        qtile.screens[target_screen].setGroup(current_group)

    return _inner


def focus_or_switch(group_name):
    '''
    Focus the selected group on the current screen or switch to the other
    screen if the group is currently active there
    '''
    @lazy.function
    def _inner(qtile):
        # Check what groups are currently active
        groups = [s.group.name for s in qtile.screens]

        try:
            # Jump to that screen if we are active
            index = groups.index(group_name)
            qtile.toScreen(index)
        except ValueError:
            # We're not active so pull the group to the current screen
            qtile.currentScreen.setGroup(qtile.groupMap[group_name])

    return _inner


def to_scratchpad(window):
    '''
    Mark the current window as a scratchpad. This resises it, sets it to
    floating and moves it to the hidden `scratchpad` group.
    '''
    try:
        window.togroup('scratchpad')
        window.on_scratchpad = True
    except Exception as e:
        # No `scratchpad` group
        notify((
            'You are attempting to use scratchpads without a `scratchpad`'
            ' group being defined! Define one in your config and restart'
            ' qtile to enable scratchpads.'
        ))

    window.floating = True
    screen = window.group.screen

    window.tweak_float(
        x=int(screen.width / 10),
        y=int(screen.height / 10),
        w=int(screen.width / 1.2),
        h=int(screen.height / 1.2),
        )


def show_scratchpad(qtile):
    '''
    Cycle through any current scratchpad windows on the current screen.
    '''
    scratchpad = qtile.groupMap.get('scratchpad')
    if scratchpad is None:
        notify((
            'You are attempting to use scratchpads without a `scratchpad`'
            ' group being defined! Define one in your config and restart'
            ' qtile to enable scratchpads.'
        ))

    for w in list(qtile.currentGroup.windows):
        if not hasattr(w, 'on_scratchpad'):
            # Ensure that we don't get an attribute error
            w.on_scratchpad = False

        if w.on_scratchpad:
            w.togroup('scratchpad')

    if scratchpad.focusHistory:
        # We have at least one scratchpad window to display so show that last
        # one to be focused. This will cause us to cycle through all scratchpad
        # windows in reverse order.
        last_window = scratchpad.focusHistory[-1]
        last_window.togroup(qtile.currentGroup.name)


# qtile actually has an emacs style `EzKey` helper that makes specifying
# key bindings a lot nicer than the default.
keys = [EzKey(k[0], *k[1:]) for k in [
    # .: Movement :.
    # Swtich focus between panes
    ("M-<Up>", lazy.layout.up()),
    ("M-<Down>", lazy.layout.down()),
    ("M-<Left>", lazy.layout.left()),
    ("M-<Right>", lazy.layout.right()),

    ("M-h", lazy.layout.left()),
    ("M-j", lazy.layout.down()),
    ("M-k", lazy.layout.up()),
    ("M-l", lazy.layout.right()),

    ("M-z", lazy.layout.down()),

    # Swap panes: target relative to active.
    # NOTE :: The `swap` commands are for XMonad
    ("M-S-<Up>", lazy.layout.shuffle_up()),
    ("M-S-<Down>", lazy.layout.shuffle_down()),
    ("M-S-<Left>", lazy.layout.shuffle_left(), lazy.layout.swap_left()),
    ("M-S-<Right>", lazy.layout.shuffle_right(), lazy.layout.swap_right()),

    ("M-S-h", lazy.layout.shuffle_left(), lazy.layout.swap_left()),
    ("M-S-j", lazy.layout.shuffle_down()),
    ("M-S-k", lazy.layout.shuffle_up()),
    ("M-S-l", lazy.layout.shuffle_right(), lazy.layout.swap_right()),

    # Grow/shrink the main the focused window
    # NOTE :: grow/shrink for XMonadTall, grow_X for Wmii/BSP
    ("M-C-<Up>", lazy.layout.grow_up()),
    ("M-C-<Down>", lazy.layout.grow_down()),
    ("M-C-<Left>", lazy.layout.grow_left()),
    ("M-C-<Right>", lazy.layout.grow_right()),

    ("M-C-k", lazy.layout.grow_up()),
    ("M-C-j", lazy.layout.grow_down()),
    ("M-C-h", lazy.layout.grow_left()),
    ("M-C-l", lazy.layout.grow_right()),

    # .: Xmonad :. #
    # Swap the position of the master/child panes
    ("M-<backslash>", lazy.layout.flip()),
    ("M-<minus>", lazy.layout.shrink()),
    ("M-<equal>", lazy.layout.grow()),

    # .: BSP :. #
    ("M-<period>", lazy.layout.toggle_split()),
    ("M-A-<Up>", lazy.layout.flip_up()),
    ("M-A-<Down>", lazy.layout.flip_down()),
    ("M-A-<Left>", lazy.layout.flip_left()),
    ("M-A-<Right>", lazy.layout.flip_right()),

    ("M-A-k", lazy.layout.flip_up()),
    ("M-A-j", lazy.layout.flip_down()),
    ("M-A-h", lazy.layout.flip_left()),
    ("M-A-k", lazy.layout.flip_right()),

    # .: Program Launchers :. #
    ("M-<Return>", lazy.spawn(TERMINAL + " -e zsh")),
    ("M-C-w", lazy.spawn(TERMINAL + ' -e "weechat"')),
    ("M-C-t", lazy.spawncmd()),

    # Easy Access
    ("M-r", lazy.spawn("thunar")),
    ("M-w", lazy.spawn("rofi -show run")),

    # Scratchpad toggles
    ("M-<slash>", lazy.group['scratchpad'].dropdown_toggle('term')),
    ("M-m", lazy.group['scratchpad'].dropdown_toggle('radio')),

    # .: Layout / Focus Manipulation :. #
    ("M-f", lazy.window.toggle_fullscreen()),
    # Toggle between the available layouts.
    ("M-<Tab>", lazy.next_layout()),
    # Switch focus between two screens
    ("M-s", lazy.to_screen(0)),
    ("M-a", lazy.to_screen(1)),
    ("M-d", lazy.to_screen(2)),
    # Move the focused group to one of the screens and follow it
    ("M-S-s", switch_screens(0), lazy.to_screen(0)),
    ("M-S-d", switch_screens(2), lazy.to_screen(2)),
    ("M-S-a", switch_screens(1), lazy.to_screen(1)),
    # Close the current window: NO WARNING!
    ("M-q", lazy.window.kill()),
    ("M-S-<BackSpace>", lazy.window.kill()),

    # .: Sys + Utils :. #
    # Restart qtile in place and pull in config changes (check config before
    # doing this with `check-qtile-conf` script to avoid crashes)
    ("M-S-r", lazy.restart()),
    # Shut down qtile.
    ("M-S-q", lazy.shutdown()),
    ("M-S-l", lazy.spawn("slock")),
    ("M-S-p", lazy.spawn("systemctl suspend")),
]]

# .: Jump between groups and also throw windows to groups :. #
for _ix, group in enumerate(groups[:10]):
    # Index from 1-0 instead of 0-9
    ix = 0 if _ix == 9 else _ix + 1

    keys.extend([EzKey(k[0], *k[1:]) for k in [
        # M-ix = switch to that group
        ("M-%d" % ix, lazy.group[group.name].toscreen()),
        # ("M-%d" % ix, focus_or_switch(group.name)),
        # M-S-ix = switch to & move focused window to that group
        ("M-S-%d" % ix, lazy.window.togroup(group.name)),
    ]])

# .: Use the mouse to drag floating layouts :. #
mouse = [
    Drag([MOD], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([MOD], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([MOD], "Button2", lazy.window.bring_to_front())
]
