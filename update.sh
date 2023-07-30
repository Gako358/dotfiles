#!/usr/bin/env bash

set -ex

echo "Updating system..."
git pull

# Get flake input from the user
echo "Please enter the flake:"
read flake

# Run the nixos-rebuild command
sudo nixos-rebuild switch --flake .#$flake

echo "System updated!"

echo "Updating home-manager..."
home_name="merrinx@$flake"

# Update home-manager
home-manager switch --flake .#$home_name
