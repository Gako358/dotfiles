{ pkgs, ... }:
let
  git = "${pkgs.git}/bin/git";
  find = "${pkgs.findutils}/bin/find";
  awk = "${pkgs.gawk}/bin/awk";
  dirname = "${pkgs.coreutils}/bin/dirname";
  mktemp = "${pkgs.coreutils}/bin/mktemp";
  rm = "${pkgs.coreutils}/bin/rm";
  gnuplot = "${pkgs.gnuplot}/bin/gnuplot";
in
pkgs.writeShellScriptBin "git-lines-graph" ''
  if [ $# -lt 4 ]; then
    echo "Usage: git-lines-graph <since> <until> <location> <author> [author...]"
    echo "  since     start date (inclusive), e.g. 2025-01-01"
    echo "  until     end date (exclusive), e.g. 2025-07-01"
    echo "  location  directory to search for repos, e.g. ~/Sources"
    echo "  author    one or more git author regexes (name or email)"
    echo ""
    echo "Produces a bar chart (git-lines-<since>_<until>.png) in the current"
    echo "directory comparing added/removed/net lines per author."
    exit 1
  fi

  since=$1
  until=$2
  location=$3
  shift 3

  data=$(${mktemp})
  trap '${rm} -f "$data"' EXIT

  for author in "$@"; do
    stats=$(${find} "$location" -type d -name .git -prune | while read -r gitdir; do
      repo=$(${dirname} "$gitdir")
      ${git} -C "$repo" log --author="$author" \
        --since="$since" --until="$until" \
        --pretty=tformat: --numstat 2>/dev/null
    done | ${awk} '{ add += $1; del += $2 } END { printf "%d %d %d", add+0, del+0, (add-del) }')
    printf '"%s" %s\n' "$author" "$stats" >> "$data"
    printf "%-30s added: %-8s removed: %-8s net: %-8s\n" \
      "$author" "''${stats%% *}" "$(echo "$stats" | ${awk} '{print $2}')" "$(echo "$stats" | ${awk} '{print $3}')"
  done

  out="git-lines-''${since}_''${until}.png"

  ${gnuplot} <<EOF
  set terminal pngcairo size 1000,600 enhanced font "sans,11"
  set output "$out"
  set title "Git lines ($since .. $until)"
  set ylabel "Lines"
  set style data histograms
  set style histogram clustered gap 1
  set style fill solid 1.0 border -1
  set boxwidth 0.9
  set grid ytics
  set xtics rotate by -30
  set key outside right top
  plot "$data" using 2:xtic(1) title "Added" lc rgb "#4caf50", \
       ""      using 3          title "Removed" lc rgb "#e53935", \
       ""      using 4          title "Net" lc rgb "#1e88e5"
  EOF

  echo ""
  echo "Graph written to $out"
''
