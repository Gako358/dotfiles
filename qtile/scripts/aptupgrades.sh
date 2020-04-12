#!/bin/bash
# Check for available updates in apt.
# NOTE :: <(cmd) is process substitution. It is different from $(cmd) as it acts like a file
#         rather than the _contents_ of a file.

# UI settings
PENDING_SYMBOL=" "
PENDING_COLOR="#d79921"
NONPENDING_COLOR="#b8bb26"

# Parse out the information we need from a simulated update
read upgraded new removed held < <(
    # Grab the update info
    aptitude full-upgrade --simulate --assume-yes |
    # Find the line that states what changes are pending
    grep -m1 '^[0-9]\+ packages upgraded' |
    # Pull of the counts in each section and return as a single space delimited line
    awk ' BEGIN { RS=",|and"; ORS=" " }; { print $1 }'
)

if [[ $upgraded != 0 ]] || [[ $new != 0 ]] || [[ $removed != 0 ]] || [[ $held != 0 ]]; then
    color="$PENDING_COLOR"
else
    color="$NONPENDING_COLOR"
fi

echo -n "<span font='12' foreground='$color'> $PENDING_SYMBOL$upgraded/$new/$removed/$held </span>"
# echo -n "$PENDING_SYMBOL$upgraded/$new/$removed/$held"
