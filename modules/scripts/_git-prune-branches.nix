{ pkgs, ... }:
let
  git = "${pkgs.git}/bin/git";
in
pkgs.writeShellScriptBin "git-prune-branches" ''
  dry_run=0
  force=0

  for arg in "$@"; do
    case "$arg" in
      -n|--dry-run) dry_run=1 ;;
      -f|--force) force=1 ;;
      -h|--help)
        echo "Usage: git-prune-branches [-n|--dry-run] [-f|--force]"
        echo "  Fetches with --prune and deletes local branches whose upstream is gone."
        echo "  -n  only list what would be deleted"
        echo "  -f  delete even if the branch is not merged"
        exit 0
        ;;
      *)
        printf "unknown option: %s\n" "$arg" >&2
        exit 1
        ;;
    esac
  done

  if ! ${git} rev-parse --git-dir >/dev/null 2>&1; then
    printf "not a git repository\n" >&2
    exit 1
  fi

  current=$(${git} branch --show-current)

  if ! ${git} fetch --all --prune; then
    printf "fetch failed\n" >&2
    exit 1
  fi

  deleted=0
  skipped=0

  while read -r branch track; do
    [ "$track" = "[gone]" ] || continue

    if [ "$branch" = "$current" ]; then
      printf "skip %s (checked out)\n" "$branch"
      skipped=$((skipped + 1))
      continue
    fi

    if [ "$dry_run" -eq 1 ]; then
      printf "would delete %s\n" "$branch"
      deleted=$((deleted + 1))
      continue
    fi

    if [ "$force" -eq 1 ]; then
      flag=-D
    else
      flag=-d
    fi

    if ${git} branch "$flag" "$branch" 2>/dev/null; then
      deleted=$((deleted + 1))
    elif [ "$force" -eq 1 ]; then
      printf "skip %s (delete failed)\n" "$branch"
      skipped=$((skipped + 1))
    else
      printf "skip %s (unmerged, use --force)\n" "$branch"
      skipped=$((skipped + 1))
    fi
  done < <(${git} for-each-ref --format='%(refname:short) %(upstream:track)' refs/heads)

  printf "\nDone. %s deleted, %s skipped.\n" "$deleted" "$skipped"
''
