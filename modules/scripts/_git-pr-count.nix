{ pkgs, ... }:
let
  git = "${pkgs.git}/bin/git";
  gh = "${pkgs.gh}/bin/gh";
  jq = "${pkgs.jq}/bin/jq";
in
pkgs.writeShellScriptBin "git-pr-count" ''
  if [ "$#" -eq 1 ] && [ "$1" = "--help" ]; then
    echo "Usage: git-pr-count <branch> <number-of-commits>"
    echo "  Counts first-parent commits on a local branch associated with merged GitHub PRs."
    exit 0
  fi

  if [ "$#" -ne 2 ]; then
    printf "Usage: git-pr-count <branch> <number-of-commits>\n" >&2
    exit 1
  fi

  branch=$1
  count=$2
  if [[ ! "$count" =~ ^[0-9]*[1-9][0-9]*$ ]]; then
    printf "number-of-commits must be a positive integer\n" >&2
    exit 1
  fi

  if ! ${git} rev-parse --git-dir >/dev/null 2>&1; then
    printf "not a git repository\n" >&2
    exit 1
  fi

  if ! ${git} check-ref-format --branch "$branch" >/dev/null 2>&1 ||
     ! ${git} show-ref --verify --quiet "refs/heads/$branch"; then
    printf "local branch not found: %s\n" "$branch" >&2
    exit 1
  fi

  commits=$(${git} log --first-parent -n "$count" --format=%H "refs/heads/$branch") || exit 1
  merged=0
  total=0
  while IFS= read -r sha; do
    response=$(${gh} api "repos/{owner}/{repo}/commits/$sha/pulls") || exit 1
    is_pr=$(printf '%s' "$response" | ${jq} -r --arg branch "$branch" \
      'if any(.[]; .merged_at != null and .base.ref == $branch) then 1 else 0 end') || exit 1
    merged=$((merged + is_pr))
    total=$((total + 1))
  done <<< "$commits"

  printf "%s of %s commits on %s were merged via pull request.\n" "$merged" "$total" "$branch"
''
