#!/usr/bin/env bash

# Source your setup script
source ./scripts/createFS.sh

# Call setup_layout with the argument passed to the script
setup_layout $1
