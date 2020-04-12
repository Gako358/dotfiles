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

from settings import MOD, TERMINAL, ACME_SCRIPT_DIR
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

    # .: Xmonad :. #
    # ("M-<slash>", lazy.layout.maximize()),
    # ("M-S-<slash>", lazy.layout.normalize()),
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

    # .: Program Launchers :. #
    ("M-<Return>", lazy.spawn(TERMINAL + " -e zsh")),
    ("M-<semicolon>", lazy.spawn('rofi-apps')),
    ("M-d", lazy.spawn(
        "dmenu_run -b -p 'λ' -sb '#83a598' -nb '#504945' -nf '#ebdbb2'")),
    ("M-n", lazy.spawn('rofi-wifi-menu')),
    ("M-r", lazy.spawncmd()),  # Quick execution of shell commands
    ("M-w", lazy.spawn('rofi -show window')),
    ("M-C-a", lazy.spawn("acme")),
    ("M-C-c", lazy.spawn("chromium-browser")),
    ("M-C-e", lazy.spawn("emacs")),
    ("M-C-f", lazy.spawn("firefox")),
    ("M-C-i", lazy.spawn("python3.6 -m qtconsole")),
    ("M-C-S-i", lazy.spawn("python3.7 -m qtconsole")),
    ("M-C-r", lazy.spawn(TERMINAL + ' -e "ranger"')),
    ("M-C-t", lazy.spawn("thunar")),
    ("M-C-w", lazy.spawn(TERMINAL + ' -e "weechat"')),

    # Scratchpad toggles
    ("M-<slash>", lazy.group['scratchpad'].dropdown_toggle('term')),
    ("M-S-<slash>", lazy.group['scratchpad'].dropdown_toggle('ipython')),
    # ("M-<slash>", lazy.window.function(to_scratchpad)),
    # ("M-S-<slash>", lazy.function(show_scratchpad)),

    # .: Layout / Focus Manipulation :. #
    ("M-f", lazy.window.toggle_fullscreen()),
    # Toggle between the available layouts.
    ("M-<grave>", lazy.next_layout()),
    ("A-<grave>", lazy.prev_layout()),
    # Switch focus between two screens
    ("M-<bracketleft>", lazy.to_screen(0)),
    ("M-<bracketright>", lazy.to_screen(1)),
    # Move the focused group to one of the screens and follow it
    ("M-S-<bracketleft>", switch_screens(0), lazy.to_screen(0)),
    ("M-S-<bracketright>", switch_screens(1), lazy.to_screen(1)),
    # Toggle between the two most recently used groups
    # TODO :: Write my own version of this that has the same
    #         screen preserving behaviour
    ("M-<Tab>", lazy.screen.toggle_group()),
    # Close the current window: NO WARNING!
    ("M-S-q", lazy.window.kill()),
    ("M-S-<BackSpace>", lazy.window.kill()),

    # .: Sys + Utils :. #
    # Restart qtile in place and pull in config changes (check config before
    # doing this with `check-qtile-conf` script to avoid crashes)
    ("M-A-r", lazy.restart()),
    # Shut down qtile.
    ("M-A-<Escape>", lazy.shutdown()),
    ("M-A-l", lazy.spawn("lock-screen")),
    ("M-A-s", lazy.spawn("screenshot")),
    ("M-A-<Delete>", lazy.spawn(script("power-menu.sh"))),

    # Acme editor shortcuts
    ("M-o", lazy.spawn(os.path.join(ACME_SCRIPT_DIR, "afindfile.sh"))),
    ("M-s", lazy.spawn(os.path.join(
        ACME_SCRIPT_DIR, "acme-fuzzy-window-search.sh"))),
]]

# .: Jump between groups and also throw windows to groups :. #
for _ix, group in enumerate(groups[:10]):
    # Index from 1-0 instead of 0-9
    ix = 0 if _ix == 9 else _ix + 1

    keys.extend([EzKey(k[0], *k[1:]) for k in [
        # M-ix = switch to that group
        # ("M-%d" % ix, lazy.group[group.name].toscreen()),
        ("M-%d" % ix, focus_or_switch(group.name)),
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
