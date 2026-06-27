#!/usr/bin/env bash
#
# NixOS installer with sops-nix bootstrap.
#
# Why this script exists:
#   sops-nix derives each host's age identity from /etc/ssh/ssh_host_ed25519_key.
#   With impermanence, that path is bind-mounted from /persist/etc/ssh/.
#   On a fresh install /persist is empty, so sops-nix has no key to derive from
#   and `nixos-install` fails during activation.
#
#   This script seeds /mnt/persist/etc/ssh/ with a host key BEFORE running
#   `nixos-install`, so sops-nix finds what it needs on first activation.

set -euo pipefail

select_machine() {
    if [ -n "${1:-}" ]; then
        machine="$1"
        if [ ! -d "./modules/hosts/$machine" ]; then
            echo "Unknown host: $machine"
            echo "Available hosts:"
            ls ./modules/hosts
            exit 1
        fi
        echo "Target host: $machine"
        read -r -p "Continue with installation? (y/n): " confirm
        if [[ "$confirm" != "y" && "$confirm" != "Y" ]]; then
            echo "Installation aborted."
            exit 0
        fi
        return
    fi

    echo "=================================="
    echo "NixOS Installation Script"
    echo "=================================="
    echo "Please select a machine to install:"
    echo "  1) aanallein"
    echo "  2) rhuidean"
    echo "  3) tanchico"
    echo "  4) terangreal"
    echo "  5) tuathaan"
    echo "=================================="
    read -r -p "Enter your choice (1-5): " choice

    case "$choice" in
        1) machine="aanallein" ;;
        2) machine="rhuidean" ;;
        3) machine="tanchico" ;;
        4) machine="terangreal" ;;
        5) machine="tuathaan" ;;
        *) echo "Invalid choice. Please select 1-5."; exit 1 ;;
    esac

    echo "You selected: $machine"
    read -r -p "Continue with installation? (y/n): " confirm
    if [[ "$confirm" != "y" && "$confirm" != "Y" ]]; then
        echo "Installation aborted."
        exit 0
    fi
}

run_disko() {
    local disko_path="./modules/hosts/$machine/_disks.nix"
    if [ ! -f "$disko_path" ]; then
        echo "Error: Disk configuration file not found at $disko_path"
        exit 1
    fi

    echo "Starting disk formatting with disko for $machine..."
    nix --experimental-features "nix-command flakes" \
        run github:nix-community/disko -- \
        --mode zap_create_mount "$disko_path"
    echo "Disk formatting completed."
}

# ── sops bootstrap ──────────────────────────────────────────────────────────
#
# After disko, /mnt is the new system's root and /mnt/persist is the persisted
# subvolume. We need /mnt/persist/etc/ssh/ssh_host_ed25519_key to exist so that
# sops-nix can derive an age identity from it during `nixos-install`.

ensure_persist_mounted() {
    if ! mountpoint -q /mnt/persist; then
        echo "Error: /mnt/persist is not a mountpoint. Did disko complete?"
        exit 1
    fi
}

generate_new_host_key() {
    local target_key="$1"

    ssh-keygen -t ed25519 -N "" -C "root@$machine" -f "$target_key"

    echo
    echo "[sops] Deriving age public key from new host key..."
    local age_pub
    age_pub=$(nix --extra-experimental-features 'nix-command flakes' \
            shell nixpkgs#ssh-to-age -c sh -c \
        "ssh-to-age < ${target_key}.pub")

    echo
    echo "=================================================================="
    echo "  ADD THIS TO .sops.yaml AND RE-ENCRYPT SECRETS BEFORE CONTINUING"
    echo "=================================================================="
    echo
    echo "  Under the 'hosts' anchor block:"
    echo
    echo "      - &${machine} ${age_pub}"
    echo
    echo "  And under creation_rules.key_groups[0].age:"
    echo
    echo "      - *${machine}"
    echo
    echo "  Then on a machine that already has the master/admin age key:"
    echo
    echo "      sops updatekeys secrets/default.yaml"
    echo "      git commit -am 'sops: add ${machine}'"
    echo "      git push"
    echo
    echo "  Finally, pull those changes into the flake checkout you are"
    echo "  installing from (the one in your current working directory)."
    echo "=================================================================="
    echo
    read -r -p "Press ENTER once .sops.yaml is updated and pulled... "
}

restore_existing_host_key() {
    local target_key="$1"
    local src_key
    read -r -p "Path to backed-up ssh_host_ed25519_key (private): " src_key

    if [ ! -f "$src_key" ] || [ ! -f "${src_key}.pub" ]; then
        echo "Error: ${src_key} or ${src_key}.pub not found."
        exit 1
    fi

    cp "$src_key"        "$target_key"
    cp "${src_key}.pub"  "${target_key}.pub"
    echo "[sops] Restored host key into $(dirname "$target_key")/."
}

restore_host_key_from_usb() {
    local target_key="$1"
    local dev mnt src
    command -v blkid >/dev/null 2>&1 || return 1

    dev=$(blkid -L SOPSKEY 2>/dev/null || true)
    [ -n "$dev" ] || return 1

    mnt=$(mktemp -d)
    if ! mount "$dev" "$mnt" 2>/dev/null; then
        rmdir "$mnt"
        return 1
    fi

    src="${mnt}/hostkeys/${machine}/ssh_host_ed25519_key"
    if [ -f "$src" ]; then
        cp "$src" "$target_key"
        [ -f "${src}.pub" ] && cp "${src}.pub" "${target_key}.pub"
        chmod 0600 "$target_key"
        [ -f "${target_key}.pub" ] && chmod 0644 "${target_key}.pub"
        umount "$mnt"; rmdir "$mnt"
        echo "[sops] Restored host key for ${machine} from installer USB."
        return 0
    fi

    umount "$mnt"; rmdir "$mnt"
    return 1
}

bootstrap_sops_host_key() {
    ensure_persist_mounted

    local persist_ssh_dir="/mnt/persist/etc/ssh"
    local target_key="${persist_ssh_dir}/ssh_host_ed25519_key"

    install -d -m 0755 /mnt/persist/etc
    install -d -m 0755 "$persist_ssh_dir"

    if [ -f "$target_key" ]; then
        echo "[sops] Existing host key found at $target_key — re-using."
        return
    fi

    if restore_host_key_from_usb "$target_key"; then
        return
    fi

    echo
    echo "=================================="
    echo "sops bootstrap for $machine"
    echo "=================================="
    echo "No SSH host key in /mnt/persist/etc/ssh/."
    echo
    echo "  1) Generate a NEW key (for a brand-new host not yet in .sops.yaml)."
    echo "  2) RESTORE an existing key from backup (host already in .sops.yaml)."
    echo "  3) SKIP bootstrap (only do this if service.sops.enable = false"
    echo "     in this host's _machine.nix; otherwise nixos-install will fail)."
    echo
    read -r -p "Choice [1/2/3]: " key_choice

    case "$key_choice" in
        1) generate_new_host_key "$target_key" ;;
        2) restore_existing_host_key "$target_key" ;;
        3) echo "[sops] Skipping bootstrap."; return ;;
        *) echo "Invalid choice."; exit 1 ;;
    esac

    chmod 0600 "$target_key"
    chmod 0644 "${target_key}.pub"
}

# ── sops secondary (master/admin) age key ───────────────────────────────────
#
# The system sops config sets keyFile = /etc/sops/age/keys.txt as an optional
# secondary identity, bind-mounted early from /persist/etc/sops. A configured-
# but-missing keyFile is fatal to sops-install-secrets, so seed it from the
# installer's master key (SOPS_AGE_KEY_FILE, loaded from the SOPSKEY USB).

seed_sops_master_key() {
    ensure_persist_mounted

    # Locate the master age key from whichever flow we're in: the installer's
    # exported env (build-installer USB), the live user's sops dir, or a
    # manually-mounted secrets USB.
    local src=""
    for cand in \
        "${SOPS_AGE_KEY_FILE:-}" \
        "/home/nixos/.config/sops/age/keys.txt" \
        "$HOME/.config/sops/age/keys.txt" \
        "/mnt/usb/keys.txt" \
        "/mnt/usb/master-age-key.txt"; do
        if [ -n "$cand" ] && [ -f "$cand" ]; then
            src="$cand"
            break
        fi
    done

    if [ -z "$src" ]; then
        echo "[sops] No master age key found (set SOPS_AGE_KEY_FILE or place it"
        echo "       on the secrets USB). Skipping /etc/sops seed — first boot's"
        echo "       sops will fail unless this host's sops.age.keyFile is unset."
        return
    fi
    echo "[sops] Using master age key from $src"

    local dest_dir="/mnt/persist/etc/sops/age"
    local dest_key="${dest_dir}/keys.txt"

    if [ -f "$dest_key" ]; then
        echo "[sops] Existing key at $dest_key — re-using."
        return
    fi

    install -d -m 0755 /mnt/persist/etc
    install -d -m 0700 /mnt/persist/etc/sops
    install -d -m 0700 "$dest_dir"
    install -m 0600 "$src" "$dest_key"
    echo "[sops] Seeded master age key -> $dest_key"
}

# ── Secure Boot signing keys (lanzaboote) ───────────────────────────────────
#
# If the target host enables Secure Boot, lanzaboote signs the boot files
# during `nixos-install` and needs its PKI bundle (/var/lib/sbctl) to exist.
# We generate a fresh per-install key (enrollment into firmware is a manual
# step anyway) and seed it into:
#   - /mnt/persist/var/lib/sbctl : the persisted copy, bind-mounted on the
#     running system so `nixos-rebuild` can keep re-signing across wipes.
#   - /mnt/var/lib/sbctl         : the ephemeral root copy, so the install-time
#     signing step finds the keys (impermanence wipes this on first boot).
# Hosts without lanzaboote skip this entirely.

seed_secureboot_keys() {
    ensure_persist_mounted

    local sb_enabled
    sb_enabled=$(nix --experimental-features "nix-command flakes" \
            eval --json ".#nixosConfigurations.${machine}.config.boot.lanzaboote.enable" \
        2>/dev/null || echo "false")
    if [ "$sb_enabled" != "true" ]; then
        echo "[secureboot] lanzaboote not enabled for ${machine} — skipping key seed."
        return
    fi

    local pki="/var/lib/sbctl"
    if [ ! -d "$pki/keys" ]; then
        echo "[secureboot] Generating fresh Secure Boot signing keys..."
        sbctl create-keys
    else
        echo "[secureboot] Re-using existing sbctl keys at $pki."
    fi

    install -d -m 0700 /mnt/persist/var/lib
    [ -d /mnt/persist/var/lib/sbctl ] || cp -a "$pki" /mnt/persist/var/lib/sbctl
    install -d -m 0700 /mnt/var/lib
    [ -d /mnt/var/lib/sbctl ] || cp -a "$pki" /mnt/var/lib/sbctl

    echo "[secureboot] Seeded signing keys into /mnt/persist/var/lib/sbctl and /mnt/var/lib/sbctl."
    echo "[secureboot] After first boot: put firmware in Setup Mode, then run"
    echo "             'sbctl enroll-keys --microsoft' and enable Secure Boot."
}

run_install() {
    local flake_target=".#$machine"
    echo "Installing NixOS on $machine..."
    nixos-install --flake "$flake_target" --no-root-password
    echo "NixOS installation completed successfully on $machine."
}

main() {
    select_machine "$@"
    run_disko
    bootstrap_sops_host_key
    seed_sops_master_key
    seed_secureboot_keys
    run_install
}

main "$@"
