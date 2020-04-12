"""
My helper scripts for setting up qtile
"""
from libqtile.config import Key

import subprocess
import os

from settings import SCRIPT_DIR


def run(cmd, with_output=False):
    """
    Run an external command as a subprocess, optionally return the output
    """
    if with_output:
        result = subprocess.run(cmd.split(), stdout=subprocess.PIPE)
        return result.stdout.decode()
    else:
        subprocess.run(cmd.split())


def wallpaper(fname):
    """Set the wallpaper using feh"""
    run("feh --bg-fill /home/innes/Pictures/Wallpapers/%s" % fname)


def script(fname):
    """Get the path of a script in the scripts directory"""
    return os.path.expanduser(SCRIPT_DIR + fname)


def run_script(fname, with_output=False):
    """Run a script from my scripts directory"""
    return run(script(fname), with_output=with_output)


def poll_func(script_name):
    """Used in generating the status bar"""
    def poll():
        return run_script(script_name, with_output=True)
    return poll


# XXX : Ah, looks like this is a thing already...!
def ez_keys(key_bindings):
    """
    Simplify the declaration of key bindings:
        Before :: Key([mod, "shift"], "slash", lazy.layout.maximise())
        After  :: ("M-S-/", lazy.layout.maximise())
    """
    modifiers = {"M": "mod4", "A": "mod1", "S": "shift", "C": "control"}

    special_chars = {
        "/": "slash", "\\": "backslash", ";": "semicolon", "`": "grave",
        "[": "bracketleft", "]": "bracketright", "Esc": "Escape",
        ",": "comma", ".": "period", "-": "minus", "=": "equal",
        "del": "Delete", "bckspc": "Backspace", "ret": "Return",
        "'": "quoteleft", "#": "numbersign"
    }

    new_bindings = []

    for spec, *actions in key_bindings:
        # Only the last key is pressed, the rest are held as modifiers
        *held, pressed = spec.split("-")
        # Expand the shorthands
        held = [modifiers.get(h, h) for h in held]
        pressed = special_chars.get(pressed, pressed)
        # Build the binding
        new_bindings.append(Key(held, pressed, *actions))

    return new_bindings


def notify(msg):
    """Send a notification. Used mainly for debugging config."""
    run('notify-send "qtile" "%s"' % msg, with_output=False)
