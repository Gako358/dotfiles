#!/usr/bin/env bash

# Check if profile exists
if [ -d ~/.local/state/nix/profiles ]; then
    echo "Profile exists"
else
    echo "Profile does not exist"
    mkdir -p ~/.local/state/nix/profiles
fi

# Get flake input from the user
clear
echo "Please enter the flake:"
echo "Choose from: 1: terangreal, 2: tuathaan"

read flake
if [[ "$flake" == "1" ]]
then
    flake="terangreal"
elif [[ "$flake" == "2" ]]
then
    flake="tuathaan"
else
    echo "Invalid input!"
    echo "Use 1 or 2..."
    exit 1
fi

home_name="merrinx@$flake"

# Install Home-manager
nix run github:nix-community/home-manager#home-manager -- switch --flake .#$home_name;
