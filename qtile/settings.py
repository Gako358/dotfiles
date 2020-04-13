'''
Settings, customisation and tweaks.
'''
import os


# Colour codes from gruvbox for use in the UI.
# TODO :: Parse this out of a common shell script so that colours are
#         automatically consistant with shell program pango as well.
COLS = {
    "dark_0": "#1d2021",
    "dark_1": "#282828",
    "dark_2": "#32302f",
    "dark_3": "#3c3836",
    "dark_4": "#504945",
    "dark_5": "#665c54",
    "dark_6": "#7c6f64",
    "gray_0": "#928374",
    "light_0": "#f9f5d7",
    "light_1": "#fbf1c7",
    "light_2": "#f2e5bc",
    "light_3": "#ebdbb2",
    "light_4": "#d5c4a1",
    "light_5": "#bdae93",
    "light_6": "#a89984",
    "red_0": "#fb4934",
    "red_1": "#cc241d",
    "red_2": "#9d0006",
    "green_0": "#b8bb26",
    "green_1": "#98971a",
    "green_2": "#79740e",
    "yellow_0": "#fabd2f",
    "yellow_1": "#d79921",
    "yellow_2": "#b57614",
    "blue_0": "#83a598",
    "blue_1": "#458588",
    "blue_2": "#076678",
    "purple_0": "#d3869b",
    "purple_1": "#b16286",
    "purple_2": "#8f3f71",
    "aqua_0": "#8ec07c",
    "aqua_1": "#689d6a",
    "aqua_2": "#427b58",
    "orange_0": "#fe8019",
    "orange_1": "#d65d0e",
    "orange_2": "#af3a03",
    # Additional related colors from the deus colorscheme
    'deus_1': '#2C323B',
    'deus_2': '#646D7A',
    'deus_3': '#48505D',
    'deus_4': '#1A222F',
    'deus_5': '#101A28',
}

# Modifier keys
ALT = "mod1"    # Left Alt
MOD = "mod4"    # Windows/Super
R_ALT = "mod3"  # Right Alt

# Directions
# DIRECTIONS = ("h", "j", "k", "l")
DIRECTIONS = ("Left", "Down", "Up", "Right")

# Programs
TERMINAL = "urxvt"

# UI Config vars
# FONT = 'Hack Regular'
# FONT = 'ProFont for Powerline Regular'
FONT = 'TerminessTTF Nerd Font Medium'
FOREGROUND = COLS['light_3']
ALERT = COLS['red_1']
FONTSIZE = 15
PADDING = 3

# Keep all of the UI consistent
FONT_PARAMS = {
    'font': FONT,
    'fontsize': FONTSIZE,
    'foreground': FOREGROUND,
}

# Location of my script files (must have a trailing slash!)
SCRIPT_DIR = os.path.expanduser('/home/merrinx/.config/qtile/scripts/')

# Whether or not the primary monitor should spawn a systray
# NOTE :: When embedding qtile inside of another desktop environment (such
#         as mate) this should be `False` as the DE systray and qtile's
#         end up fighting each other and both loose...!
WITH_SYS_TRAY = True
