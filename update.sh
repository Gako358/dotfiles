#!/usr/bin/env bash

set -ex


# Test system
clear
echo "Please enter the flake:"
echo "Choose from: 1: terangreal, 2: tuathaan"
# Get flake input from the user
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

echo "Do you want to test the system or home-manager?"
echo "1. System, 2. Home-manager, 3. Both or 4. None"
read test

if [[ "$test" == "1" ]]; then
    nixos-rebuild build --flake .#$flake
    exit 0
elif [[ "$test" == "2" ]]; then
    home-manager build --flake .#$home_name
    exit 0
elif [[ "$test" == "3" ]]; then
    nixos-rebuild build --flake .#$flake
    home-manager build --flake .#$home_name
    exit 0
else
    continue
fi

clear
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

# Run the nixos-rebuild command
sudo nixos-rebuild switch --flake .#$flake

clear
echo "System updated!"
echo "Updating home-manager..."

echo "Remove old generations...?"
echo "1. Yes, 2. No"
read hmgen

# Run a garbage collection
if [[ "$hmgen" == "1" ]]
then
    home-manager expire-generations "-3 days"
    continue
else
    continue
fi

# Update home-manager
home-manager switch --flake .#$home_name
