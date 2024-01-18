#!/usr/bin/env bash

ROOT_DIR=$(pwd)

activate(){
    source $ROOT_DIR/.venv/bin/activate
    python3 src/main.py
}

activate
