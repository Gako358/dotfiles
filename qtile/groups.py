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
from libqtile.config import Group, ScratchPad, DropDown
from layouts import layouts 

groups = [
        Group("1 ", {'layout': 'bsp'}),
        Group("2 ", {'layout': 'monadtall'}),
        Group("3 ", {'layout': 'monadtall'}),
        Group("4 ", {'layout': 'monadtall'}),
        Group("5 ", {'layout': 'stack'}),
        Group("6 ", {'layout': 'monadtall'}),
        Group("7 ", {'layout': 'monadwide'}),
        Group("8 λ", {'layout': 'monadwide'}),
        Group("9 ", {'layout': 'stack'}),
        Group("10 ", {'layout': 'stack'}),
    # Group("scratchpad"),
    # Scratchpads on M-/ and M-S-/
    ScratchPad("scratchpad", [
        # NOTE :: Need to force spawning as a new process so that
        #         qtile can capture the new terminal by pid.
        DropDown("term", "tilix --new-process",
                 on_focus_lost_hide=False, x=0.05, y=0.05,
                 width=0.9, height=0.9),
        DropDown("ipython", "python3.7 -m qtconsole",
                 on_focus_lost_hide=False, x=0.05, y=0.05,
                 width=0.9, height=0.9)
    ]),
]

