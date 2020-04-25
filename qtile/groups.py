'''
qtile calls i3-style workspaces `groups`.

Groups are a little more powerful as we can specify additional config
to apply to each group if we want:

NOTE :: Match is imported from libqtile.config
>>> Group(
        # Display name for the group
        name="my-workspace",
        # Capture spawned programs and move them to this group
        matches=[Match(wm_class=["FireFox"])],
        # Spawn these programs on start
        spawn=["my-program", "my-other-program"],
        # Layout to use (must be in the listed layouts)
        layout="MonadTall",
        # Should this group exist even when there are no windows?
        persist=True,
        # Create this group when qtile starts?
        init=True
    )
'''
from libqtile.config import Group, ScratchPad, DropDown, Match, Rule
from layouts import layouts 

groups = [
    Group("1 "),
    Group("2 ", init=True, persist=True, spawn='urxvt -e ytop',
        matches=[Match(wm_class=['urxvt -e ytop'])], layout="monadwide"
        ),
    Group("3 ", init=True, persist=True,
        matches=[Match(wm_class=['lyx'])], exclusive=False, layout="monadtall"
        ),
    Group("4 ", init=True, persist=True, spawn='urxvt -e weechat', 
        matches=[Match(wm_class=['urxvt -e weechat'])], layout="monadtall"
        ),
    Group("5 ", init=True, persist=True, spawn='chromium',
        matches=[Match(wm_class=['Chromium'])], exclusive=True, layout="stack"
        ),
    Group("6 ", init=True, persist=True, spawn='mailspring',
        matches=[Match(wm_class=['Mailspring'])], exclusive=True, layout="stack"
        ),
    Group("7 "),
    Group("8 ", init=True, persist=True,
        matches=[Match(wm_class=['Virt-manager'])], exclusive=True, layout="stack"
        ),
    Group("9 ", init=True, persist=True, spawn='discord',
        matches=[Match(wm_class=['discord'])], exclusive=True, layout="stack"
        ),
    Group("10 ", init=False, persist=True,
        matches=[Match(wm_class=['Steam'])], exclusive=True, layout="stack"
        ),
    # Group("scratchpad"),
    # Scratchpads on M-/ and M-S-/
    ScratchPad("scratchpad", [
        # NOTE :: Need to force spawning as a new process so that
        #         qtile can capture the new terminal by pid.
        DropDown("term", "urxvt",
                 on_focus_lost_hide=False, x=0.05, y=0.05,
                 width=0.9, height=0.9),
        DropDown("radio", "urxvt -e curseradio",
                 on_focus_lost_hide=False, x=0.05, y=0.05,
                 width=0.9, height=0.9)
    ]),
]

dgroups_app_rules = [
    # Everything i want to be float, but don't want to change group
    Rule(Match(title=['nested', 'gscreenshot'],
               wm_class=['Guake.py', 'Exe', 'gimp', 'thunar', 'Onboard', 'Florence',
                         'Plugin-container', 'Terminal', 'Gpaint',
                         'Kolourpaint', 'Wrapper', 'Gcr-prompter',
                         'Ghost', 'feh', 'Gnuplot', 'Pinta', 
                         'Gnome-keyring-prompt'],
               ),
         float=True, intrusive=True),

    # floating windows
    Rule(Match(wm_class=['Synfigstudio', 'Wine', 'Xephyr', 'postal2-bin', 'File Manager']
               ),
         float=True),
    ]

