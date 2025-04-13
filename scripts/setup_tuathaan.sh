
#!/usr/bin/env bash

set -e  # Exit immediately if a command exits with a non-zero status

echo "Starting disk formatting with disko..."
nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode zap_create_mount ./hosts/tuathaan/disko.nix

# Check if disko completed successfully
if [ $? -eq 0 ]; then
    echo "Disk formatting completed successfully."
    echo "Installing NixOS..."
    nixos-install --flake .#tuathaan --no-root-password
else
    echo "Error: Disk formatting failed. Aborting installation."
    exit 1
fi

echo "NixOS installation completed."
