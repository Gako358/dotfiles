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
  - `tuathaan`: ASUS laptop
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


## Via install media
You can also install this via the install media in the nix-install repo by doing the following:

- Boot off the install media.
- Create the partition schedule and mount it to /mnt
- Run `nixos-install --flake github:gako358/dotfiles#terangreal



## NixOS wsl
1.  Configure Nix: If you're using Nix to manage packages in your NixOS WSL environment, you'll need to configure it to use the proxy as well. Create or edit the /etc/nix/nix.conf file (you might need to use sudo to edit this file) and add the following lines, replacing <proxy_address> and <proxy_port> with the correct values:

```
http_proxy = http://<proxy_address>:<proxy_port>
https_proxy = https://<proxy_address>:<proxy_port>
```

2.  Set environment variables: Configure the proxy settings by setting the http_proxy, https_proxy, and ftp_proxy environment variables in your NixOS WSL environment.

To set the environment variables temporarily for the current session, run the following commands in the WSL terminal, replacing `<proxy_address>` and `<proxy_port>` with the correct values:

```
export http_proxy=http://<proxy_address>:<proxy_port>
export https_proxy=https://<proxy_address>:<proxy_port>
export ftp_proxy=ftp://<proxy_address>:<proxy_port>
```

To set the environment variables permanently, add the above lines to your NixOS WSL shell's configuration file (e.g., ~/.bashrc or ~/.zshrc) and restart the terminal.

3.  Reset WSL network: You can try resetting the WSL network by running the following command in PowerShell as an administrator:
```
wsl.exe --terminate <DistroName>
```

## SSL peer certificate

1. Obtain the proxy SSL certificate: Request the proxy SSL certificate from your network administrator. The certificate should be in PEM format (`.pem`, `.crt`, or `.cer` file). If the certificate is in a different format, you may need to convert it using tools like `openssl`.

2. Import the proxy SSL certificate into the Nix store:

```
nix-store --add-fixed sha256 /path/to/proxy_cert.pem
```

1. Configure Nix to use the proxy SSL certificate:
edit `/etc/nix/nix.conf`

```
sudo nano /etc/nix/nix.conf
```

```
http_proxy = http://my-proxy.address.com:8080
https_proxy = http://my-proxy.address.com:8080
ssl_ca_file = /nix/store/abcdefgh1234567890abcdef-proxy_cert.pem
```
