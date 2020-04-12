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


# Named Groups copied from i3
# >>> See https://fontawesome.com/cheatsheet for more fontawesome icons
groups = [
    Group("1 "),
    Group("2 "),
    Group("3 "),
    Group("4 "),
    Group("5 "),
    Group("6 "),
    Group("7 "),
    Group("8 λ"),
    Group("9 "),
    Group("10 "),
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

# Simple numbered groups
# groups = [Group(str(x+1)) for x in range(10)]

# Roman numerals + icons
# groups = [
#     Group("I "),
#     Group("II "),
#     Group("III "),
#     Group("IV "),
#     Group("V "),
#     Group("VI "),
#     Group("VII "),
#     Group("VIII λ"),
#     Group("IX "),
#     Group("X "),
# ]

# Roman numerals only
# groups = [
#     Group("I"),
#     Group("II"),
#     Group("III"),
#     Group("IV"),
#     Group("V"),
#     Group("VI"),
#     Group("VII"),
#     Group("VIII"),
#     Group("IX"),
#     Group("X"),
# ]

# Icons only
# groups = [
#     Group(""),
#     Group(""),
#     Group(""),
#     Group(""),
#     Group(""),
#     Group(""),
#     Group(""),
#     Group("λ"),
#     Group(""),
#     Group(""),
# ]

# Named
# groups = [
#     Group("TERM"),
#     Group("NOTES"),
#     Group("DOCS"),
#     Group("SCRATCH"),
#     Group("COMS"),
#     Group("WEB"),
#     Group("CODE"),
#     Group("REPL"),
#     Group("MUSIC"),
#     Group("GAMES"),
# ]
