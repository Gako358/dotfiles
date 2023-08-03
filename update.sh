#!/usr/bin/env bash

set -ex

echo "Updating system..."
git pull

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
    exit 1
fi

# Run the nixos-rebuild command
sudo nixos-rebuild switch --flake .#$flake

echo "System updated!"

echo "Updating home-manager..."
home_name="merrinx@$flake"

# Update home-manager
home-manager switch --flake .#$home_name
