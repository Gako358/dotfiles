{ pkgs, ... }:
let
  git = "${pkgs.git}/bin/git";
  find = "${pkgs.findutils}/bin/find";
  dirname = "${pkgs.coreutils}/bin/dirname";
  basename = "${pkgs.coreutils}/bin/basename";
in
pkgs.writeShellScriptBin "git-pull-all" ''
  location=''${1:-.}

  ok=0
  failed=0

  while read -r gitdir; do
    repo=$(${dirname} "$gitdir")
    name=$(${basename} "$repo")
    printf "==> %s\n" "$name"

    if ${git} -C "$repo" show-ref --verify --quiet refs/heads/master; then
      branch=master
    elif ${git} -C "$repo" show-ref --verify --quiet refs/heads/main; then
      branch=main
    else
      failed=$((failed + 1))
      printf "    ✗ no master/main branch, skipping %s\n" "$name"
      continue
    fi

    if ! ${git} -C "$repo" checkout "$branch"; then
      failed=$((failed + 1))
      printf "    ✗ checkout %s failed for %s\n" "$branch" "$name"
      continue
    fi

    if ${git} -C "$repo" pull --rebase --autostash; then
      ok=$((ok + 1))
    else
      failed=$((failed + 1))
      printf "    ✗ pull --rebase failed for %s\n" "$name"
    fi
  done < <(${find} "$location" -type d -name .git -prune)

  printf "\nDone. %s ok, %s failed.\n" "$ok" "$failed"
''
