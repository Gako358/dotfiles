{ pkgs, ... }:
let
  awk = "${pkgs.gawk}/bin/awk";
  cat = "${pkgs.coreutils}/bin/cat";
  coredumpctl = "${pkgs.systemd}/bin/coredumpctl";
  cut = "${pkgs.coreutils}/bin/cut";
  grep = "${pkgs.gnugrep}/bin/grep";
  id = "${pkgs.coreutils}/bin/id";
  journalctl = "${pkgs.systemd}/bin/journalctl";
  mktemp = "${pkgs.coreutils}/bin/mktemp";
  rm = "${pkgs.coreutils}/bin/rm";
  sed = "${pkgs.gnused}/bin/sed";
  sort = "${pkgs.coreutils}/bin/sort";
  systemctl = "${pkgs.systemd}/bin/systemctl";
  uname = "${pkgs.coreutils}/bin/uname";
  uniq = "${pkgs.coreutils}/bin/uniq";
  uptime = "${pkgs.procps}/bin/uptime";
in
pkgs.writeShellScriptBin "system-check" /* bash */ ''
  set -uo pipefail

  ESC=$(printf '\033')
  if [ -t 1 ]; then
    RED="''${ESC}[0;31m"
    YELLOW="''${ESC}[1;33m"
    DIM="''${ESC}[2m"
    BOLD="''${ESC}[1m"
    NC="''${ESC}[0m"
  else
    RED=""; YELLOW=""; DIM=""; BOLD=""; NC=""
  fi

  since=""
  limit=10

  while [ $# -gt 0 ]; do
    case "$1" in
      -s|--since)
        shift
        since=''${1:-}
        if [ -z "$since" ]; then
          printf "--since needs a value\n" >&2
          exit 1
        fi
        ;;
      -f|--full) limit=0 ;;
      -h|--help)
        echo "Usage: system-check [-s|--since <time>] [-f|--full]"
        echo ""
        echo "Collects errors first, then warnings, from failed units, the"
        echo "journal, the kernel, Hyprland, quickshell, GNOME and coredumps."
        echo "Identical messages are grouped with a count; digits are replaced"
        echo "by N so that pids, ids and store hashes collapse into one entry."
        echo ""
        echo "  -s, --since <time>  window in journalctl syntax; default: this boot"
        echo "  -f, --full          do not cap entries per source (default: 10)"
        exit 0
        ;;
      *)
        printf "unknown option: %s\n" "$1" >&2
        exit 1
        ;;
    esac
    shift
  done

  [ -n "$since" ] || since=$(${uptime} -s)
  runtime=''${XDG_RUNTIME_DIR:-/run/user/$(${id} -u)}

  errors=$(${mktemp})
  warnings=$(${mktemp})
  group=$(${mktemp})
  trap '${rm} -f "$errors" "$warnings" "$group"' EXIT

  strip_ansi() {
    ${sed} -E "s/''${ESC}\[[0-9;]*[a-zA-Z]//g"
  }

  clean() {
    strip_ansi \
      | ${sed} -E \
        -e 's#/nix/store/[a-z0-9]+-#/nix/store/#g' \
        -e 's/[0-9]+/N/g' \
        -e 's/[[:space:]]+/ /g' \
        -e 's/^ //' \
      | ${cut} -c -160
  }

  record() {
    ${grep} -av '^-- ' | clean | ${grep} -av '^$' \
      | ${sort} | ${uniq} -c | ${sort} -rn > "$group"
    if [ -s "$group" ]; then
      {
        printf '## %s\n' "$2"
        ${cat} "$group"
      } >> "$1"
    fi
  }

  failed_units() {
    ${systemctl} "$@" --failed --no-legend --plain 2>/dev/null \
      | ${awk} '{ unit = $1; $1 = $2 = $3 = $4 = ""; sub(/^ +/, ""); print unit ": " $0 }'
  }

  journal_at() {
    ${journalctl} --since "$since" -p "$1" -o short-iso --no-pager 2>/dev/null \
      | ${cut} -d' ' -f3- \
      | ${grep} -av '^kernel: '
  }

  kernel_at() {
    ${journalctl} -k --since "$since" -p "$1" -o cat --no-pager 2>/dev/null
  }

  gnome_at() {
    ${journalctl} --since "$since" -p "$1" -o cat --no-pager \
      -t gnome-shell -t gnome-session-binary -t gdm 2>/dev/null
  }

  hyprland_at() {
    local f
    for f in "$runtime"/hypr/*/hyprland.log; do
      [ -f "$f" ] || continue
      ${grep} -a "^$1" "$f"
    done
  }

  quickshell_at() {
    ${journalctl} --user -u quickshell.service --since "$since" -o cat --no-pager 2>/dev/null \
      | strip_ansi \
      | ${grep} -aE "^ *($1)"
  }

  coredumps() {
    ${coredumpctl} list --no-legend --since "$since" 2>/dev/null \
      | ${awk} '{ print $8 ": " $10 }'
  }

  failed_units | record "$errors" "failed units (system)"
  failed_units --user | record "$errors" "failed units (user)"
  journal_at 0..3 | record "$errors" "journal"
  kernel_at 0..3 | record "$errors" "kernel"
  gnome_at 0..3 | record "$errors" "gnome"
  hyprland_at ERR | record "$errors" "hyprland"
  quickshell_at 'ERROR|CRITICAL|FATAL' | record "$errors" "quickshell"
  coredumps | record "$errors" "coredumps"

  journal_at 4..4 | record "$warnings" "journal"
  kernel_at 4..4 | record "$warnings" "kernel"
  gnome_at 4..4 | record "$warnings" "gnome"
  hyprland_at WARN | record "$warnings" "hyprland"
  quickshell_at 'WARN' | record "$warnings" "quickshell"

  print_bucket() {
    printf '\n%s%s%s\n' "$BOLD$2" "$1" "$NC"
    if [ ! -s "$3" ]; then
      printf '  %snone%s\n' "$DIM" "$NC"
      return
    fi
    ${awk} -v limit="$limit" -v color="$2" -v dim="$DIM" -v nc="$NC" '
      function flush() {
        if (extra > 0) printf "      %s... %d more%s\n", dim, extra, nc
      }
      /^## / {
        flush()
        printf "\n  %s\n", substr($0, 4)
        shown = 0
        extra = 0
        next
      }
      {
        if (limit > 0 && shown >= limit) { extra++; next }
        count = $1
        $1 = ""
        sub(/^ +/, "")
        printf "    %s%5sx%s %s\n", color, count, nc, $0
        shown++
      }
      END { flush() }
    ' "$3"
  }

  state=$(${systemctl} is-system-running 2>/dev/null)

  printf '%ssystem-check%s  %s  since %s  systemd: %s\n' \
    "$BOLD" "$NC" "$(${uname} -n)" "$since" "''${state:-unknown}"

  print_bucket "ERRORS" "$RED" "$errors"
  print_bucket "WARNINGS" "$YELLOW" "$warnings"

  err_groups=$(${grep} -avc '^## ' "$errors")
  warn_groups=$(${grep} -avc '^## ' "$warnings")

  printf '\n%s%s error group(s), %s warning group(s)%s\n' \
    "$DIM" "$err_groups" "$warn_groups" "$NC"
''
