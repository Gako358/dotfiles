#!/usr/bin/env bash

sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode zap_create_mount ./host/disko.nix
