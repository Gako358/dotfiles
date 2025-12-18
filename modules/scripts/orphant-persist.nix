{ pkgs
, ...
}:

pkgs.writeShellScriptBin "find-orphant" /* bash */ ''
  # Colors for output
  RED='\033[0;31m'
  GREEN='\033[0;32m'
  YELLOW='\033[1;33m'
  NC='\033[0m' # No Color

  PERSIST_PATH="/persist/home/merrinx"

  # Get all mounted paths under /home/merrinx (both direct and fuse mounts)
  echo -e "''${GREEN}Checking for unmounted items in $PERSIST_PATH...''${NC}\n"

  # Create temporary file for mounted paths
  MOUNTED_PATHS=$(mktemp)

  echo "Debug: Getting mount points..." >&2

  # Get mount points from /proc/mounts or mount command (more reliable than df)
  # This will catch both regular mounts and fuse mounts
  mount | grep "/home/merrinx" | awk '{print $3}' | while read -r mountpoint; do
      # Get the corresponding persist path
      if [[ "$mountpoint" =~ ^/home/merrinx/(. *)$ ]]; then
          echo "$PERSIST_PATH/''${BASH_REMATCH[1]}"
      fi
  done | sort -u > "$MOUNTED_PATHS"

  # Also check using findmnt which handles fuse mounts better
  if command -v findmnt &> /dev/null; then
      findmnt -rn -o TARGET | grep "^/home/merrinx" | while read -r mountpoint; do
          if [[ "$mountpoint" =~ ^/home/merrinx/(.*)$ ]]; then
              echo "$PERSIST_PATH/''${BASH_REMATCH[1]}"
          fi
      done | sort -u >> "$MOUNTED_PATHS"
  fi

  # Remove duplicates and save
  sort -u "$MOUNTED_PATHS" -o "$MOUNTED_PATHS"

  echo "Debug: Found $(wc -l < "$MOUNTED_PATHS") mounted paths" >&2
  echo "Debug: Starting to scan persist directory..." >&2

  # Find all items (files and directories) in persist path
  UNMOUNTED_ITEMS=()

  # Simple approach - just check each item directly
  for item in "$PERSIST_PATH"/* "$PERSIST_PATH"/.*; do
      # Skip .  and ..  and glob patterns
      [[ "$item" == "$PERSIST_PATH/." ]] && continue
      [[ "$item" == "$PERSIST_PATH/.." ]] && continue
      [[ "$item" == "$PERSIST_PATH/*" ]] && continue
      [[ "$item" == "$PERSIST_PATH/.*" ]] && continue
      [[ !  -e "$item" ]] && continue

      echo "Debug: Checking $item" >&2

      # Check if this exact path is mounted
      if ! grep -q "^$item$" "$MOUNTED_PATHS"; then
          # For directories, check if it's a parent of mounted paths
          if [[ -d "$item" ]]; then
              is_parent=false
              while read -r mounted; do
                  if [[ "$mounted" == "$item/"* ]]; then
                      is_parent=true
                      break
                  fi
              done < "$MOUNTED_PATHS"

              [[ "$is_parent" == true ]] && continue
          fi

          UNMOUNTED_ITEMS+=("$item")
      fi
  done

  echo "Debug:  Checking subdirectories..." >&2

  # Now check inside . config, .local, .cache for unmounted items
  for parent_dir in "$PERSIST_PATH/.config" "$PERSIST_PATH/.local" "$PERSIST_PATH/.cache"; do
      [[ ! -d "$parent_dir" ]] && continue

      echo "Debug: Checking inside $parent_dir" >&2

      for item in "$parent_dir"/* "$parent_dir"/.*; do
          [[ "$item" == "$parent_dir/." ]] && continue
          [[ "$item" == "$parent_dir/.." ]] && continue
          [[ "$item" == "$parent_dir/*" ]] && continue
          [[ "$item" == "$parent_dir/.*" ]] && continue
          [[ ! -e "$item" ]] && continue

          echo "Debug:    - Checking $item" >&2

          # Check if this specific item is mounted
          if ! grep -q "^$item$" "$MOUNTED_PATHS"; then
              # Check if parent is mounted
              parent_mounted=false
              temp_parent="$item"
              while [[ "$temp_parent" != "$PERSIST_PATH" ]]; do
                  temp_parent=$(dirname "$temp_parent")
                  if grep -q "^$temp_parent$" "$MOUNTED_PATHS"; then
                      parent_mounted=true
                      break
                  fi
              done

              if [[ "$parent_mounted" == false ]]; then
                  echo "Debug:     -> Not mounted!" >&2
                  UNMOUNTED_ITEMS+=("$item")
              else
                  echo "Debug:     -> Parent is mounted" >&2
              fi
          else
              echo "Debug:     -> Is mounted" >&2
          fi
      done
  done

  # Also check . local/share subdirectory
  if [[ -d "$PERSIST_PATH/.local/share" ]]; then
      echo "Debug: Checking inside $PERSIST_PATH/.local/share" >&2

      for item in "$PERSIST_PATH/.local/share"/* "$PERSIST_PATH/.local/share"/.*; do
          [[ "$item" == "$PERSIST_PATH/.local/share/." ]] && continue
          [[ "$item" == "$PERSIST_PATH/.local/share/.." ]] && continue
          [[ "$item" == "$PERSIST_PATH/.local/share/*" ]] && continue
          [[ "$item" == "$PERSIST_PATH/.local/share/.*" ]] && continue
          [[ ! -e "$item" ]] && continue

          echo "Debug:   - Checking $item" >&2

          # Check if this specific item is mounted
          if ! grep -q "^$item$" "$MOUNTED_PATHS"; then
              # Check if parent is mounted
              parent_mounted=false
              temp_parent="$item"
              while [[ "$temp_parent" != "$PERSIST_PATH" ]]; do
                  temp_parent=$(dirname "$temp_parent")
                  if grep -q "^$temp_parent$" "$MOUNTED_PATHS"; then
                      parent_mounted=true
                      break
                  fi
              done

              [[ "$parent_mounted" == false ]] && UNMOUNTED_ITEMS+=("$item")
          fi
      done
  fi

  echo "Debug: Processing complete" >&2

  # Display results
  if [[ ''${#UNMOUNTED_ITEMS[@]} -eq 0 ]]; then
      echo -e "''${GREEN}✓ All items in $PERSIST_PATH are properly mounted! ''${NC}"
  else
      echo -e "''${YELLOW}Found ''${#UNMOUNTED_ITEMS[@]} unmounted item(s):''${NC}\n"

      for item in "''${UNMOUNTED_ITEMS[@]}"; do
          # Get item type and info
          if [[ -d "$item" ]]; then
              TYPE="''${YELLOW}[DIR]''${NC} "
              SIZE=$(du -sh "$item" 2>/dev/null | cut -f1)
          elif [[ -f "$item" ]]; then
              TYPE="''${RED}[FILE]''${NC}"
              SIZE=$(du -h "$item" 2>/dev/null | cut -f1)
          else
              TYPE="[OTHER]"
              SIZE="N/A"
          fi

          # Get relative path for cleaner output
          REL_PATH="''${item#$PERSIST_PATH/}"

          printf "%b %-50s %8s\n" "$TYPE" "$REL_PATH" "($SIZE)"
      done

      echo -e "\n''${RED}These items exist in persist but are not mounted to your home directory!''${NC}"
      echo -e "''${YELLOW}You may want to add them to your NixOS persistence configuration. ''${NC}"
  fi

  # Debug: Show what we detected as mounted (optional)
  if [[ -s "$MOUNTED_PATHS" ]]; then
      echo -e "\n''${GREEN}Debug:  Detected mounted persist paths:''${NC}"
      while read -r path; do
          REL_PATH="''${path#$PERSIST_PATH/}"
          [[ -n "$REL_PATH" ]] && echo "  → $REL_PATH"
      done < "$MOUNTED_PATHS"
  fi

  # Clean up temp file
  rm -f "$MOUNTED_PATHS"
''
