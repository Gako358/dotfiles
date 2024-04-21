#!/usr/bin/env bash

set -ex


# Test system
clear

flake=$(hostname)
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

# delete older generations
nix-collect-garbage --delete-older-than 28d

git pull

# Run a garbage collection
# nix-collect-garbage

# Run the nixos-rebuild command
sudo nixos-rebuild switch --flake .#$flake

clear
echo "System updated!"
echo "Updating home-manager..."

# Run a garbage collection
home-manager expire-generations "-19 days"

# Update home-manager
home-manager switch --flake .#$home_name
