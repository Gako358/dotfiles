[![NixOS Unstable](https://img.shields.io/badge/NixOS-unstable-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

> **Disclaimer:** _This is not a community framework or distribution._ It's a
> private configuration and an ongoing experiment to feel out NixOS. I make no
> guarantees that it will work out of the box for anyone but myself. It may also
> change drastically and without warning.

# My NixOS Configuration

Welcome to my compilation of dotfiles, the secret sauce behind the construction and configuration of my Linux systems.

For a deeper dive into NixOS, the innovative Linux distribution I use, and Nix, the powerful package management tool and language that this repository is primarily written in, click [Nix](https://nixos.org/).

## Sneak Peek

![screenshot](https://github.com/Gako358/archive/blob/main/images/config/work.png)

> **Neovim Users** _My Neovim flake is available:_ [here](https://github.com/Gako358/neovim)

# How To Install

There are two main ways to deploy these dotfiles on a system:

### On an existing NixOS system

If you have setup a NixOS system with a configuration.nix file its possible to switch over to this nix config with
the following commands:

```shell
nix-shell
nixos-rebuild switch --flake .#
```

`Note: This assumes your computer name matches one of the configurations in the flake.`

## Repository Structure

- `flake.nix`: Entrypoint for hosts and home configurations. Also exposes a
  devshell for boostrapping (`nix develop` or `nix-shell`).
- `hosts`: NixOS Configurations, accessible via `nixos-rebuild --flake`.
  - `common`: Shared configurations consumed by the machine-specific ones.
  - `terangreal`: Desktop PC - 48GB RAM, Ryzon7 5600
  - `tuathaan`: HP work laptop
  - `sangreal`: ASUS laptop
  - `angreal`: WSL for work
- `home`: My Home-manager configuration, acessible via `home-manager --flake`
  - Each directory here is a "feature" each hm configuration can toggle, thus
    customizing my setup for each machine (be it a server, desktop, laptop,
    anything really).
- `modules`: Modules that is a "feature", each that can be toggled on or off in `flake.nix`
- `overlay`: Patches and version overrides for some packages. Accessible via
  `nix build`.
- `pkgs`: My custom packages. Also accessible via `nix build`. You can compose
  these into your own configuration by using my flake's overlay, or consume them through NUR.
- `templates`: A couple project templates for different languages. Accessible
  via `nix init`.
  - `C` - C develop environment
  - `devshell` - Basic develop shell, to be bootstapped to anything
  - `latex` - Latex develop environment
  - `python` - Python develop environment
  - `rust` - Rust develop environment
  - `wasm` - wasm develop environment
  - `java` - Java develop environment

### Installation via Media

Alternatively, you can install these configurations via the install media from the nix-install repo as follows:

- Boot off the install media.
- Create the partition schedule and mount it to /mnt
- Run `nixos-install --flake github:gako358/dotfiles#terangreal
