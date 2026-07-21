{ pkgs, ... }:
let
  git = "${pkgs.git}/bin/git";
  find = "${pkgs.findutils}/bin/find";
  awk = "${pkgs.gawk}/bin/awk";
  dirname = "${pkgs.coreutils}/bin/dirname";
in
pkgs.writeShellScriptBin "git-lines-total" ''
  if [ $# -lt 3 ]; then
    echo "Usage: git-lines-total <author> <since> <until> [location]"
    echo "  author    git author regex (name or email), e.g. gako358"
    echo "  since     start date (inclusive), e.g. 2025-01-01"
    echo "  until     end date (exclusive), e.g. 2025-07-01"
    echo "  location  directory to search for repos (default: current dir)"
    exit 1
  fi

  author=$1
  since=$2
  until=$3
  location=''${4:-.}

  ${find} "$location" -type d -name .git -prune | while read -r gitdir; do
    repo=$(${dirname} "$gitdir")
    ${git} -C "$repo" log --author="$author" \
      --since="$since" --until="$until" \
      --pretty=tformat: --numstat 2>/dev/null
  done | ${awk} '{ add += $1; del += $2 } END { printf "added: %s  removed: %s  net: %s\n", add+0, del+0, (add-del) }'
''
