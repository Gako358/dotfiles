[![NixOS Unstable](https://img.shields.io/badge/NixOS-unstable-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

> **Disclaimer:** _This is not a community framework or distribution._ It's a
> private configuration and an ongoing experiment to feel out NixOS. I make no
> guarantees that it will work out of the box for anyone but myself. It may also
> change drastically and without warning.

# NIXOS Configuration
This is my collection of dot files which I use to build my Linux systems and
control how they are configured.

For more information on NixOS the Linux distribution I use and also nix
the packaging tool and language that most of this repository is writen in
[Nix](https://nixos.org/)

## Screenshot
![screenshot](https://github.com/Gako358/archive/blob/main/images/config/work.png)

# HowTo
These dot files can be installed onto a system by 1 of two ways:

## Already running nixos system
If you have setup a nixos system with a configuration.nix file its possible to switch over to this nix config with
the following commands:

```shell
nix-shell
nixos-rebuild switch --flake .#
```

The above assuse your computer name matches one of the configurations in the flake.

## Structure

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
    -  `C` - C develop environment
    -  `devshell` - Basic develop shell, to be bootstapped to anything
    -  `latex` - Latex develop environment
    -  `python` - Python develop environment
    -  `rust` - Rust develop environment
    -  `wasm` - wasm develop environment
    -  `java` - Java develop environment


## Via install media
You can also install this via the install media in the nix-install repo by doing the following:

- Boot off the install media.
- Create the partition schedule and mount it to /mnt
- Run `nixos-install --flake github:gako358/dotfiles#terangreal

## Templates
To use, secify the template
```
nix flake new -t github:gako358/dotfiles/templates#java
```


## NixOS wsl
1.  Configure Nix: If you're using Nix to manage packages in your NixOS WSL environment, you'll need to configure it to use the proxy as well. Create or edit the /etc/nix/nix.conf file (you might need to use sudo to edit this file) and add the following lines, replacing <proxy_address> and <proxy_port> with the correct values:

```
http_proxy = http://<proxy_address>:<proxy_port>
https_proxy = https://<proxy_address>:<proxy_port>
```

### ENVIRONMENT
2. Set the env for systemd
```
#!/usr/bin/env bash

proxy_server="www-proxy.address-to-use.com"
proxy_port="8080"

# Create the directory for the Nix daemon service override
mkdir -p /run/systemd/system/nix-daemon.service.d/

# Create the override.conf file with the appropriate proxy settings
cat << EOF >/run/systemd/system/nix-daemon.service.d/override.conf
[Service]
Environment="http_proxy=http://$proxy_server:$proxy_port"
Environment="https_proxy=http://$proxy_server:$proxy_port"
Environment="ftp_proxy=http://$proxy_server:$proxy_port"
EOF

# Reload the systemd daemon and restart the Nix daemon
systemctl daemon-reload
systemctl restart nix-daemon.service

# Print success message
echo "Proxy settings and SSL certificate applied successfully for Nix daemon."
```
### WSL config
3. Edit /etc/wsl.conf

```
generateResolveConf=false
```

### NIX config
4. Edit /etc/nix/nix.conf

Add:
```
ssl-cert-file = /etc/ssl/certs/ca-bundle.crt
```

