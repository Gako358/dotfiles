import os
import subprocess

from settings import SCRIPT_DIR

from libqtile.widget import base


class ShellScript(base.ThreadedPollText):
    '''
    A generic text widget that polls using a poll function to get its text
    and accepts mouse interaction through the bar.
    When clicked, the script will be able to pull out the following env vars:
        WIDGET_BUTTON - (see below)
        WIDGET_X_LOC  - X location in pixels
        WIDGET_Y_LOC  - Y location in pixels

    Mouse interactions come through as ints mapped as follows
        1: LEFT
        2: RIGHT
        3: MIDDLE
        4: SCROLL_UP
        5: SCROLL_DOWN
    '''
    orientations = base.ORIENTATION_HORIZONTAL
    defaults = [
        ('fname', None, 'Filename in script directory'),
        ('script_dir', SCRIPT_DIR, 'Directory containing the script'),
    ]

    def __init__(self, **config):
        base.ThreadedPollText.__init__(self, **config)
        self.add_defaults(ShellScript.defaults)
        self.fname = self.script_dir + config['fname']

    def poll(self):
        '''When polled just run the script without click info'''
        return self._run_script()

    def _run_script(self, btn=None, x=None, y=None):
        '''Run the script, optionally passing click info in the environment'''
        if btn is not None:
            btn_env = os.environ.copy()
            btn_env['WIDGET_BUTTON'] = str(btn)
            btn_env['WIDGET_X_LOC'] = str(x)
            btn_env['WIDGET_Y_LOC'] = str(y)
            result = subprocess.run(
                self.fname, stdout=subprocess.PIPE, env=btn_env)
        else:
            result = subprocess.run(self.fname, stdout=subprocess.PIPE)

        return result.stdout.decode()

    def button_press(self, x, y, button):
        '''
        Pass the information off to the script but ignore the script output
        '''
        return self._run_script(btn=button, x=x, y=y)
