#!/usr/bin/env bash

set -e

install_nixos() {
    echo "=================================="
    echo "NixOS Installation Script"
    echo "=================================="
    echo "Please select a machine to install:"
    echo "1) shitbox"
    echo "=================================="
    read -r -p "Enter your choice (1-5): " choice

    case $choice in
        1)
            machine="shitbox"
            ;;
        *)
            echo "Invalid choice. Please select 1-5."
            exit 1
            ;;
    esac

    echo "You selected: $machine"
    read -r -p "Continue with installation? (y/n): " confirm
    if [[ "$confirm" != "y" && "$confirm" != "Y" ]]; then
        echo "Installation aborted."
        exit 0
    fi

    flake_target=".#$machine"
    disko_path="./hosts/$machine/disks.nix"

    if [ ! -f "$disko_path" ]; then
        echo "Error: Disk configuration file not found at $disko_path"
        exit 1
    fi

    echo "Starting disk formatting with disko for $machine..."
    if nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode zap_create_mount "$disko_path"; then
        echo "Disk formatting completed successfully."
        echo "Installing NixOS on $machine..."
        if nixos-install --flake "$flake_target" --no-root-password; then
            echo "NixOS installation completed successfully on $machine."
        else
            echo "Error: NixOS installation failed."
            exit 1
        fi
    else
        echo "Error: Disk formatting failed. Aborting installation."
        exit 1
    fi
}

install_nixos
