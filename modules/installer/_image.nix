{
  pkgs,
  lib,
  config,
  host,
  flakeSrc,
  ...
}:
let
  install-system = pkgs.writeShellScriptBin "install-system" ''
    set -euo pipefail

    src=/etc/dotfiles
    dest="$HOME/dotfiles"
    if [ ! -e "$dest" ]; then
      echo ">> Copying flake to $dest ..."
      cp -r --no-preserve=mode,ownership "$src" "$dest"
      chmod -R u+w "$dest"
    fi
    cd "$dest"

    if [ ! -f "$HOME/.config/sops/age/keys.txt" ]; then
      load-sops-key || echo "(continuing without a sops master key on this installer)"
    fi

    export SOPS_AGE_KEY_FILE="$HOME/.config/sops/age/keys.txt"
    exec sudo --preserve-env=SOPS_AGE_KEY_FILE bash ./scripts/install.sh "${host}"
  '';

  load-sops-key = pkgs.writeShellScriptBin "load-sops-key" ''
    set -euo pipefail

    dev=$(sudo blkid -L SOPSKEY || true)
    if [ -z "$dev" ]; then
      echo "No SOPSKEY partition found on any attached disk." >&2
      exit 1
    fi

    mnt=$(mktemp -d)
    sudo mount "$dev" "$mnt"
    mkdir -p "$HOME/.config/sops/age"
    sudo cp "$mnt/keys.txt" "$HOME/.config/sops/age/keys.txt"
    sudo chown "$(id -u):$(id -g)" "$HOME/.config/sops/age/keys.txt"
    chmod 600 "$HOME/.config/sops/age/keys.txt"
    sudo umount "$mnt"
    rmdir "$mnt"
    echo "Loaded sops age key -> ~/.config/sops/age/keys.txt"
  '';
in
{
  system.stateVersion = lib.mkDefault config.system.nixos.release;

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  networking.hostName = "installer-${host}";

  environment.systemPackages = [
    install-system
    load-sops-key
  ]
  ++ (with pkgs; [
    git
    disko
    sops
    age
    ssh-to-age
    jq
    gptfdisk
    dosfstools
    util-linux
  ]);

  environment.etc.dotfiles.source = flakeSrc;

  systemd.services.load-sops-key = {
    description = "Load sops age master key from the SOPSKEY USB partition";
    wantedBy = [ "multi-user.target" ];
    after = [ "local-fs.target" ];
    serviceConfig.Type = "oneshot";
    path = with pkgs; [
      util-linux
      coreutils
    ];
    script = ''
      set -eu
      dev=$(blkid -L SOPSKEY || true)
      [ -n "$dev" ] || { echo "no SOPSKEY partition present"; exit 0; }
      mnt=$(mktemp -d)
      mount "$dev" "$mnt"
      if [ -f "$mnt/keys.txt" ]; then
        install -d -m 0700 -o nixos -g users /home/nixos/.config/sops/age
        install -m 0600 -o nixos -g users "$mnt/keys.txt" \
          /home/nixos/.config/sops/age/keys.txt
      fi
      umount "$mnt"
      rmdir "$mnt"
    '';
  };

  users.motd = ''

    ┌─ NixOS installer — target host: ${host}
    │
    │  1. (optional) confirm network:   ping -c1 nixos.org
    │  2. install the system:           install-system
    │       → formats disks with disko and installs '${host}'
    │
    │  The sops master key is auto-loaded from the USB's SOPSKEY
    │  partition if present; reload manually with:  load-sops-key
    └─
  '';
}
