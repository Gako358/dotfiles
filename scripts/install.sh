#!/usr/bin/env bash

set -ex

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

if [[ "$flake" == "terangreal" ]]
then
    # Source your setup script
    source ./scripts/setup_terangreal.sh
    nixos-install --flake .#$flake

elif [[ "$flake" == "tuathaan" ]]
then
    # Source your setup script
    source ./scripts/setup_tuathaan.sh
    nixos-install --flake .#$flake
fi
