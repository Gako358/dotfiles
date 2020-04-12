'''
My group/workspace layouts.

The built in layouts can be found here:
    http://docs.qtile.org/en/latest/manual/ref/layouts.html

Look at porting some Xmonad layouts to qtile:
    https://github.com/xmonad/xmonad-contrib/blob/master/XMonad/Layout/Circle.hs
    https://github.com/xmonad/xmonad-contrib/blob/master/XMonad/Layout/Cross.hs

>>  Try cribbing from what the XmonadTall layout does:
        http://qtile.readthedocs.io/en/latest/_modules/libqtile/layout/xmonad.html#MonadTall
'''
from settings import COLS, FONT_PARAMS
from libqtile import layout


# Annoyingly, there isn't a common subset of parameters for all layouts that
# can be passed as a dict splat. There _are_ some common ones for multiple
# layouts, so they are defined here and used where possible to give a
# consistent UI.
BORDER_NORMAL = COLS["dark_2"]
# BORDER_FOCUS = COLS["blue_2"]
BORDER_FOCUS = COLS["red_1"]
BORDER_WIDTH = 3
MARGIN = 10


layouts = [
    # XXX : Emulating BSPWM (but not matching it) setting fair=False will
    #       cause
    layout.Bsp(
        border_normal=BORDER_NORMAL,
        border_focus=BORDER_FOCUS,
        border_width=BORDER_WIDTH,
        margin=MARGIN,
        fair=False,
    ),
    # XXX : My default layout. Single window fills the screen and it can
    #       keep a stack of secondary windows off to the side quite easily.
    layout.MonadTall(
        border_normal=BORDER_NORMAL,
        border_focus=BORDER_FOCUS,
        border_width=BORDER_WIDTH,
        margin=MARGIN,
        ratio=0.7,
    ),
    # XXX : Same idea as MonadTall but the smaller windows are along the
    #       top/bottom of the main window
    layout.MonadWide(
        border_normal=BORDER_NORMAL,
        border_focus=BORDER_FOCUS,
        border_width=BORDER_WIDTH,
        margin=MARGIN,
        ratio=0.9,
    ),
    # XXX : Good for browser style flipping between windows when working on
    #       large coding projects (beats constant buffer/tab swaps in Vim!)
    layout.TreeTab(
        inactive_fg=COLS["light_0"],
        inactive_bg=BORDER_NORMAL,
        active_bg=COLS["light_3"],
        active_fg=BORDER_NORMAL,
        sections=["    .: Windows :."],
        # Want a consistant font w. the terminal here
        foreground=FONT_PARAMS["foreground"],
        fontsize=FONT_PARAMS["fontsize"],
        font="ProFontWindows Nerd Font Mono Book",
    ),
    # XXX : Emulate Wmii tiling: each new window adds to the focused
    #       column. Moving a window "out" of the current colmun creates
    #       a new column.
    #layout.Wmii(
    #    border_normal=BORDER_NORMAL,
    #    border_focus=BORDER_FOCUS,
    #    border_width=BORDER_WIDTH,
    #    margin=MARGIN,
    #),
    # XXX : A simple grid fill of the screen aiming for square number tilings
    #       at the expense of leaving blank positions if it correctly places
    #       the remaining windows.
    # layout.Matrix(
    #     border_normal=BORDER_NORMAL,
    #     border_focus=BORDER_FOCUS,
    #     border_width=BORDER_WIDTH,
    #     margin=MARGIN,
    # ),
    # XXX: Split the screen according to a given ratio. Kind of tricky to
    #      know exactly what it will do without experimenting...!
    # layout.RatioTile(
    #     border_normal=BORDER_NORMAL,
    #     border_focus=BORDER_FOCUS,
    #     border_width=BORDER_WIDTH,
    #     margin=MARGIN,
    #     ratio=2.5
    # ),
]

# Specification for auto floating windows: this isn't a layout in the same
# way as the ones listed above.
floating_layout = layout.Floating(
    border_normal=BORDER_NORMAL,
    border_focus=BORDER_FOCUS,
    border_width=BORDER_WIDTH,
    float_rules=[
        {'wmclass': 'confirm'},
        {'wmclass': 'dialog'},
        {'wmclass': 'download'},
        {'wmclass': 'error'},
        {'wmclass': 'file_progress'},
        {'wmclass': 'notification'},
        {'wmclass': 'splash'},
        {'wmclass': 'toolbar'},
        {'wmclass': 'gcr-prompter'},
        {'wmclass': 'confirmreset'},
        {'wmclass': 'makebranch'},
        {'wmclass': 'maketag'},
        {'wmclass': 'peek'},
        {'wname': 'branchdialog'},
        {'wname': 'pinentry'},
        {'wmclass': 'ssh-askpass'},
    ]
)
