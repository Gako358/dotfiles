{ pkgs, ... }:
let
  git = "${pkgs.git}/bin/git";
  gh = "${pkgs.gh}/bin/gh";
  jq = "${pkgs.jq}/bin/jq";
in
pkgs.writeShellScriptBin "git-pr-review-time" ''
  if [ "$#" -eq 1 ] && [ "$1" = "--help" ]; then
    echo "Usage: git-pr-review-time <branch> <number-of-prs>"
    echo "  Shows review-request-to-first-approval turnaround for recent merged PRs."
    exit 0
  fi

  if [ "$#" -ne 2 ]; then
    printf "Usage: git-pr-review-time <branch> <number-of-prs>\n" >&2
    exit 1
  fi

  branch=$1
  count=$2
  if [[ ! "$count" =~ ^[0-9]*[1-9][0-9]*$ ]]; then
    printf "number-of-prs must be a positive integer\n" >&2
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

  repo=$(${gh} repo view --json nameWithOwner --jq .nameWithOwner) || exit 1
  owner=''${repo%/*}
  name=''${repo#*/}
  if [ -z "$owner" ] || [ -z "$name" ] || [ "$repo" = "$name" ]; then
    printf "could not determine GitHub repository\n" >&2
    exit 1
  fi

  query='query($owner: String!, $name: String!, $number: Int!, $endCursor: String) {
    repository(owner: $owner, name: $name) {
      pullRequest(number: $number) {
        reviews(first: 100, after: $endCursor) {
          nodes { state submittedAt }
          pageInfo { hasNextPage endCursor }
        }
      }
    }
  }'

  commits=$(${git} log --first-parent --format=%H "refs/heads/$branch") || exit 1
  declare -A seen=()
  found=0
  while IFS= read -r sha && [ "$found" -lt "$count" ]; do
    prs=$(${gh} api "repos/{owner}/{repo}/commits/$sha/pulls") || exit 1
    number=$(printf '%s' "$prs" | ${jq} -r --arg branch "$branch" \
      '[.[] | select(.merged_at != null and .base.ref == $branch) | .number] | first // empty') || exit 1
    if [ -z "$number" ] || [ -n "''${seen[$number]+x}" ]; then
      continue
    fi
    seen[$number]=1
    found=$((found + 1))

    events=$(${gh} api --paginate --slurp "repos/{owner}/{repo}/issues/$number/events?per_page=100") || exit 1
    reviews=$(${gh} api graphql --paginate --slurp -f query="$query" \
      -f owner="$owner" -f name="$name" -F number="$number") || exit 1
    timing=$(${jq} -nr --argjson events "$events" --argjson reviews "$reviews" '
      [$events[][] | select(.event == "review_requested" and .created_at != null)
       | .created_at] | sort | first as $request
      | [$reviews[].data.repository.pullRequest.reviews.nodes[]
         | select(.state == "APPROVED" and .submittedAt != null)
         | .submittedAt | select($request != null and . >= $request)] | sort | first as $approval
      | if $request == null then "-\t-\tno review request"
        elif $approval == null then "\($request)\t-\tno approval after request"
        else ($request | fromdateiso8601) as $start
           | ($approval | sub("\\.[0-9]+Z$"; "Z") | fromdateiso8601) as $end
           | "\($request)\t\($approval)\t\(($end - $start) / 60 | floor)m \(($end - $start) % 60)s"
        end') || exit 1
    printf '%s\t#%s\t%s\n' "$sha" "$number" "$timing"
  done <<< "$commits"
''
