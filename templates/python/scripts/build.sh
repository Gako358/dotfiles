#!/usr/bin/env bash

ROOT_DIR=$(pwd)

build(){
    python3 -m venv .venv
    source $ROOT_DIR/.venv/bin/activate
    printf "\nInstalling required packages\n"
    pip install -r dep/requirements
    printf "\nDone setting up...\n"
}

build
printf "\n\n To run the simulation: make run\n"
