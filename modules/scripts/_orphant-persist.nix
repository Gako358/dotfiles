{
  pkgs,
  ...
}:
let
  findmnt = "${pkgs.util-linux}/bin/findmnt";
  jq = "${pkgs.jq}/bin/jq";
  du = "${pkgs.coreutils}/bin/du";
  rm = "${pkgs.coreutils}/bin/rm";
in
pkgs.writeShellScriptBin "find-orphant" /* bash */ ''
  set -uo pipefail

  RED='\033[0;31m'
  GREEN='\033[0;32m'
  YELLOW='\033[1;33m'
  BOLD='\033[1m'
  NC='\033[0m'

  PERSIST_PATH="/persist/home/merrinx"
  HOME_PREFIX="/home/merrinx"

  DRY_RUN=false
  for arg in "$@"; do
    case "$arg" in
      -n|--dry-run) DRY_RUN=true ;;
      -h|--help)
        echo "Usage: find-orphant [--dry-run]"
        echo ""
        echo "Lists items under $PERSIST_PATH that are NOT backed by a live"
        echo "impermanence bind-mount (i.e. leftovers from programs no longer"
        echo "persisted), then prompts to delete each one."
        echo ""
        echo "  -n, --dry-run   only list, never prompt or delete"
        exit 0 ;;
      *) echo "Unknown argument: $arg (try --help)" >&2; exit 1 ;;
    esac
  done

  if [ ! -d "$PERSIST_PATH" ]; then
    echo -e "''${RED}Persist path $PERSIST_PATH does not exist''${NC}" >&2
    exit 1
  fi

  # --- Ground truth: the set of live bind-mount sources ---
  # findmnt --json emits clean, unescaped targets, so paths with spaces
  # (e.g. ".config/Proton Pass") compare correctly. Targets are translated
  # from the live mount point (/home/merrinx/...) into their persist source
  # (/persist/home/merrinx/...).
  mapfile -t MOUNTS < <(
    ${findmnt} --json -o TARGET 2>/dev/null \
      | ${jq} -r '[.. | .target? // empty] | .[]' \
      | grep "^$HOME_PREFIX/" \
      | sed "s|^$HOME_PREFIX|$PERSIST_PATH|" \
      | sort -u
  )

  # Safety: if we could not read any mounts, refuse — otherwise every file
  # would look orphaned and a delete-all would be catastrophic.
  if [ ''${#MOUNTS[@]} -eq 0 ]; then
    echo -e "''${RED}Refusing to run: found no bind-mounts under $HOME_PREFIX.''${NC}" >&2
    echo -e "''${RED}Without a known-good mount set, every item would look orphaned.''${NC}" >&2
    exit 1
  fi

  # Is $1 exactly a live mount source?
  is_mount() {
    local p=$1 m
    for m in "''${MOUNTS[@]}"; do
      [ "$m" = "$p" ] && return 0
    done
    return 1
  }

  # Is $1 a strict ancestor of some live mount source?
  is_ancestor_of_mount() {
    local p=$1 m
    for m in "''${MOUNTS[@]}"; do
      case "$m" in "$p"/*) return 0 ;; esac
    done
    return 1
  }

  # Is a live mount located at $1 or anywhere beneath it?
  covers_a_mount() {
    local p=$1 m
    for m in "''${MOUNTS[@]}"; do
      [ "$m" = "$p" ] && return 0
      case "$m" in "$p"/*) return 0 ;; esac
    done
    return 1
  }

  ORPHANS=()
  shopt -s dotglob nullglob

  # Classify every child: live source -> keep; container of a live mount ->
  # descend; neither -> orphan (and stop, deleting it takes everything under it).
  walk() {
    local dir=$1 child
    for child in "$dir"/*; do
      if is_mount "$child"; then
        continue
      elif is_ancestor_of_mount "$child"; then
        walk "$child"
      else
        ORPHANS+=("$child")
      fi
    done
  }

  echo -e "''${GREEN}Scanning $PERSIST_PATH for orphans (items with no live persist mount)...''${NC}\n"
  walk "$PERSIST_PATH"

  if [ ''${#ORPHANS[@]} -eq 0 ]; then
    echo -e "''${GREEN}✓ No orphans — everything in persist is backed by a live mount.''${NC}"
    exit 0
  fi

  # --- Report ---
  echo -e "''${YELLOW}Found ''${#ORPHANS[@]} orphan item(s):''${NC}\n"
  for item in "''${ORPHANS[@]}"; do
    if [ -L "$item" ]; then
      TYPE="''${YELLOW}[LINK]''${NC}"; SIZE="-"
    elif [ -d "$item" ]; then
      TYPE="''${YELLOW}[DIR] ''${NC}"; SIZE=$(${du} -sh "$item" 2>/dev/null | cut -f1)
    elif [ -f "$item" ]; then
      TYPE="''${RED}[FILE]''${NC}"; SIZE=$(${du} -h "$item" 2>/dev/null | cut -f1)
    else
      TYPE="[????]"; SIZE="-"
    fi
    printf "%b  %-50s %8s\n" "$TYPE" "''${item#$PERSIST_PATH/}" "($SIZE)"
  done

  echo -e "\n''${YELLOW}These exist in persist but no current config persists them.''${NC}"

  if [ "$DRY_RUN" = true ]; then
    echo -e "''${GREEN}(dry-run: nothing deleted)''${NC}"
    exit 0
  fi

  if [ ! -t 0 ]; then
    echo -e "''${YELLOW}(non-interactive shell: not prompting for deletion)''${NC}"
    exit 0
  fi

  # Delete a single orphan, with defense-in-depth checks that must never
  # trigger given correct detection, but guard against catastrophe anyway.
  delete_one() {
    local item=$1
    if [ -z "$item" ] || [ "$item" = "/" ] || [ "$item" = "$PERSIST_PATH" ]; then
      echo -e "  ''${RED}refused: suspicious path ($item)''${NC}"; return 1
    fi
    case "$item" in
      "$PERSIST_PATH"/*) : ;;
      *) echo -e "  ''${RED}refused: $item is outside $PERSIST_PATH''${NC}"; return 1 ;;
    esac
    if covers_a_mount "$item"; then
      echo -e "  ''${RED}refused: a live mount exists at/under $item''${NC}"; return 1
    fi
    if ${rm} -rf -- "$item"; then
      echo -e "  ''${GREEN}deleted''${NC} ''${item#$PERSIST_PATH/}"
    else
      echo -e "  ''${RED}failed to delete''${NC} ''${item#$PERSIST_PATH/}"
    fi
  }

  echo
  echo -e "''${BOLD}Delete orphans?''${NC} per item: [y]es  [n]o (default)  [a]ll remaining  [q]uit"

  ALL=false
  for item in "''${ORPHANS[@]}"; do
    if [ "$ALL" = true ]; then
      delete_one "$item"
      continue
    fi
    printf "%bdelete%b %-50s [y/N/a/q] " "$YELLOW" "$NC" "''${item#$PERSIST_PATH/}"
    read -r ans </dev/tty || ans=""
    case "$ans" in
      y|Y) delete_one "$item" ;;
      a|A) ALL=true; delete_one "$item" ;;
      q|Q) echo "aborted."; break ;;
      *)   echo "  skipped." ;;
    esac
  done
''
