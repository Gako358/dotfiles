#!/usr/bin/env bash

set -ex
echo "Run a garbage collection?"
echo "1. Yes, 2. No"
read gc

# Run a garbage collection
if [[ "$gc" == "1" ]]
then
    sudo nix-collect-garbage -d
    continue
else
    continue
fi

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
    echo "Use 1 or 2..."
    exit 1
fi

# Check if the user wants to test the flake
clear
echo "For testing flake choose <<test>>"
read test
if [[ "$test" == "test" ]]
then
    echo "Testing flake..."
    sudo nixos-rebuild build --flake .#$flake
    exit 0
else
    continue
fi

# Run the nixos-rebuild command
sudo nixos-rebuild switch --flake .#$flake

clear
echo "System updated!"
echo "Updating home-manager..."
home_name="merrinx@$flake"

# Update home-manager
home-manager switch --flake .#$home_name
