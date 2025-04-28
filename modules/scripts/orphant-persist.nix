{ pkgs
, ...
}:

pkgs.writeShellScriptBin "find-orphant" ''
  # Script to find top-level items in /persist that do not contain
  # any currently mounted sources listed by 'df'.
  # Helps identify potentially orphaned persisted directories/files
  # in NixOS impermanence setups.

  set -euo pipefail

  # --- Configuration ---
  # You can change the persist directory if yours is different
  persist_dir="/persist"
  # Set to 1 to show items that ARE needed (for debugging/verification)
  show_needed=0
  # --- End Configuration ---


  if [[ ! -d "''${persist_dir}" ]]; then
    # Use escaped double quotes instead of single quotes
    ${pkgs.coreutils}/bin/echo "Error: Persist directory \"''${persist_dir}\" not found." >&2
    exit 1
  fi

  ${pkgs.coreutils}/bin/echo "*** Finding mounted sources from ''${persist_dir} ..."

  # Get sorted list of unique source paths mounted from /persist/
  # Using mapfile is safer than command substitution with potential newlines
  # Use escaped double quotes for awk script and escape internal quotes and Nix variables
  mapfile -t mounted_sources < <(
    ${pkgs.coreutils}/bin/df -Th | \
    ${pkgs.gawk}/bin/awk -v p="''${persist_dir}/" "''${1} ~ \\\"^\\\" p {print ''${1}}" | \
    ${pkgs.coreutils}/bin/sort -u
  )


  if [[ ''${#mounted_sources[@]} -eq 0 ]]; then
    ${pkgs.coreutils}/bin/echo "No mounts found originating from ''${persist_dir}/"
    ${pkgs.coreutils}/bin/echo "All top-level items in ''${persist_dir} might be orphaned if this is unexpected:"
    # Escape {} for Nix
    ${pkgs.findutils}/bin/find "''${persist_dir}" -mindepth 1 -maxdepth 1 -exec ${pkgs.coreutils}/bin/echo "  -> {}" \;
    exit 0
  fi

  if [[ "''${show_needed}" -eq 1 ]]; then
      ${pkgs.coreutils}/bin/echo "--- Currently Mounted Sources:"
      # Escape @ for Nix array expansion
      ${pkgs.coreutils}/bin/printf "    %s\n" "''${mounted_sources[@]}"
      ${pkgs.coreutils}/bin/echo "" # Add the missing newline echo
  fi

  ${pkgs.coreutils}/bin/echo "*** Checking top-level items in ''${persist_dir} ..."

  orphaned_found=0
  # Get sorted list of top-level items (files/dirs) in /persist
  mapfile -t top_level_items < <(
      ${pkgs.findutils}/bin/find "''${persist_dir}" -mindepth 1 -maxdepth 1 | \
      ${pkgs.coreutils}/bin/sort
  )


  if [[ ''${#top_level_items[@]} -eq 0 ]]; then
    ${pkgs.coreutils}/bin/echo "No top-level items found inside ''${persist_dir}."
    exit 0
  fi

  # Escape loop variables
  for item in "''${top_level_items[@]}"; do
    item_is_needed=0
    # Ensure item path ends with / for prefix matching, unless it's the exact match
    item_prefix="''${item}/"

    for source in "''${mounted_sources[@]}"; do
      # Check if the source is the item itself OR if the source is inside the item directory
      # Use bash pattern matching for prefix check
      if [[ "''${source}" == "''${item}" ]] || [[ "''${source}" == "''${item_prefix}"* ]]; then
        item_is_needed=1
        break # No need to check other sources for this item
      fi
    done

    if [[ ''${item_is_needed} -eq 0 ]]; then
      ${pkgs.coreutils}/bin/echo "!!! Potentially ORPHANED: ''${item}"
      ${pkgs.coreutils}/bin/echo "    (No mounted sources found matching or within this path)"
      orphaned_found=1
    elif [[ "''${show_needed}" -eq 1 ]]; then
      ${pkgs.coreutils}/bin/echo "--- Needed: ''${item} (Contains or matches mounted sources)"
    fi
  done

  ${pkgs.coreutils}/bin/echo "" # Add newline echo
  if [[ ''${orphaned_found} -eq 0 ]]; then
    ${pkgs.coreutils}/bin/echo "*** No potentially orphaned top-level items found in ''${persist_dir}."
  else
    ${pkgs.coreutils}/bin/echo "*** Found potentially orphaned items listed above."
    ${pkgs.coreutils}/bin/echo "*** Please review carefully before deleting anything!"
  fi

  exit 0
''
