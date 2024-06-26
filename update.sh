#!/usr/bin/env bash

set -e

clear
echo ""
echo ""
echo ""
echo " _   _ _       ____   _____   _    _           _       _"
echo "| \\ | (_)     / __ \\ / ____| | |  | |         | |     | |"
echo "|  \\| |___  _| |  | | (___   | |  | |_ __   __| | __ _| |_ ___"
echo "| . \` | \\ \\/ / |  | |\\___ \\  | |  | | '_ \\ / _\` |/ _\` | __/ _ \\"
echo "| |\\  | |>  <| |__| |____) | | |__| | |_) | (_| | (_| | ||  __/"
echo "|_| \\_|_/_/\\_\\\\____/|_____/   \\____/| .__/ \\__,_|\\__,_|\\__\\___|"
echo "                                    | |"
echo "                                    |_|"
echo ""
echo ""

git fetch

flake=$(hostname)
home_name="merrinx@$flake"

# Check if there are any changes between the local and remote repository
if ! git diff --quiet HEAD origin/main || ! git diff --quiet; then
    echo "Changes detected, pulling changes."
    git pull >/dev/null 2>&1
else
    echo "No changes detected, exiting."
    exit 0
fi

spinner() {
    local pid=$1
    local update_message=$2
    local delay=0.75
    local spinstr='|/-\'
    printf " [ ]  $update_message...  "
    while [ "$(ps a | awk '{print $1}' | grep $pid)" ]; do
        local temp=${spinstr#?}
        printf "\b\b\b[%c]" "$spinstr"
        local spinstr=$temp${spinstr%"$temp"}
        sleep $delay
    done
    printf "\r\e[K [✓]  $update_message...  "
}

# Check disk space
disk_space=$(df /dev/nvme0n1p1 | awk 'NR==2 {print $5}' | sed 's/%//g')

# Run a garbage collection if disk space is less than 60%
if ((disk_space > 60)); then
    (sudo nix-collect-garbage -d >/dev/null 2>&1) &
    spinner $! "Collecting garbage"
    echo ""
else
    (nix-collect-garbage --delete-older-than 28d >/dev/null 2>&1) &
    spinner $! "Deleting older generations"
    echo ""
    (home-manager expire-generations "-19 days" >/dev/null 2>&1) &
    spinner $! "Removing older home generations..."
fi

# Run the nixos-rebuild command
echo ""
echo "Updating system for $flake..."
sudo -v
(sudo nixos-rebuild switch --flake .#$flake &>nixos-switch.log || (cat nixos-switch.log | grep --color error && echo "An error occurred during the rebuild. Do you want to continue? (yes/no)" && read continue && if [[ "$continue" == "no" ]]; then exit 1; fi)) &
spinner $! "System updating..."
echo ""

# Update home-manager
(home-manager switch --flake .#$home_name &>home-manager.log || (cat home-manager.log | grep --color error && echo "An error occurred during the home-manager update. Exiting." && exit 1)) &
spinner $! "Updating home"
echo ""

# Check if there were any errors during the execution of the script
if ! grep -q "error" nixos-switch.log && ! grep -q "error" home-manager.log; then
    rm nixos-switch.log home-manager.log
fi

echo ""
echo ""
echo ""
echo " ____                   "
echo "|  _ \  ___  _ __   ___ "
echo "| | | |/ _ \| '_ \ / _ \\"
echo "| |_| | (_) | | | |  __/"
echo "|____/ \___/|_| |_|\___|"
echo ""
echo ""
