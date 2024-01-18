# Makefile written by Knut Oien | knutago1@outlook.com

.PHONY: default
default: build;

ROOT_DIR=$(shell pwd)
HOSTNAME=$(shell uname -n)

dest=$(ROOT_DIR)/.venv

export ROOT_DIR

# *DOCUMENTATION*
# Running make will build the test enviroment
# and install the required dependencies

build:
	@echo -e "Hey $(HOSTNAME)"
	@echo -e "Setting up python virtual enviroment..."
	@[[ -z "$(shell ls -A -- "$(dest)")" ]] && { echo "empty"; } || { rm -r $(dest)/*; }
	@echo -e "Running build script"
	@./scripts/build.sh

clean:
	@echo -e "Cleaning up..."
	@rm -rf $(dest)

# *RUNNING*
run:
	@echo -e "Running the application"
	@./scripts/activation.sh
