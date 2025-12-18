{ pkgs
, ...
}:

pkgs.writeShellScriptBin "find-orphant" /* sh */ ''
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

  # Find all items (files and directories) in persist path
  UNMOUNTED_ITEMS=()

  # Function to check if a path or any of its children are mounted
  has_mounted_children() {
      local path="$1"
      while read -r mounted; do
          # Check if any mounted path starts with this path
          [[ "$mounted" == "$path" || "$mounted" == "$path/"* ]] && return 0
      done < "$MOUNTED_PATHS"
      return 1
  }

  # Function to check if a path is mounted
  is_mounted() {
      local path="$1"
      grep -q "^$path$" "$MOUNTED_PATHS" && return 0
      return 1
  }

  # Check top-level items
  for item in "$PERSIST_PATH"/{.*,*}; do
      # Skip . and ..  and the base path itself
      [[ "$item" == "$PERSIST_PATH/." ]] && continue
      [[ "$item" == "$PERSIST_PATH/.." ]] && continue
      [[ "$item" == "$PERSIST_PATH/*" ]] && continue  # Skip glob pattern if no match
      [[ "$item" == "$PERSIST_PATH/.*" ]] && continue  # Skip glob pattern if no match

      # Check if item exists
      [[ -e "$item" ]] || continue

      # For directories, check if they have mounted children
      if [[ -d "$item" ]]; then
          # Skip if directory itself is mounted or has mounted children
          if is_mounted "$item" || has_mounted_children "$item"; then
              continue
          fi
      fi

      # Check if this path is mounted
      if ! is_mounted "$item"; then
          UNMOUNTED_ITEMS+=("$item")
      fi
  done

  # Check subdirectories in . config, .local/share, . cache
  # Only check items that don't have parent directory already unmounted
  for parent in ". config" ".local/share" ". cache"; do
      PARENT_PATH="$PERSIST_PATH/$parent"

      # Skip if parent doesn't exist or parent is already in unmounted list
      if [[ ! -d "$PARENT_PATH" ]]; then
          continue
      fi

      # Check if parent is already marked as unmounted
      parent_unmounted=false
      for unmounted in "''${UNMOUNTED_ITEMS[@]}"; do
          if [[ "$PARENT_PATH" == "$unmounted" ]]; then
              parent_unmounted=true
              break
          fi
      done

      # If parent is unmounted, skip checking children
      if [[ "$parent_unmounted" == true ]]; then
          continue
      fi

      # Check children
      for item in "$PARENT_PATH"/{.*,*}; do
          # Skip . and ..
          [[ "$item" == "$PARENT_PATH/." ]] && continue
          [[ "$item" == "$PARENT_PATH/.." ]] && continue
          [[ "$item" == "$PARENT_PATH/*" ]] && continue
          [[ "$item" == "$PARENT_PATH/.*" ]] && continue

          # Check if item exists
          [[ -e "$item" ]] || continue

          # For directories, check if they have mounted children
          if [[ -d "$item" ]]; then
              if is_mounted "$item" || has_mounted_children "$item"; then
                  continue
              fi
          fi

          # Check if this path is mounted
          if ! is_mounted "$item"; then
              UNMOUNTED_ITEMS+=("$item")
          fi
      done
  done

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
      echo -e "\n''${GREEN}Debug: Detected mounted persist paths:''${NC}"
      while read -r path; do
          REL_PATH="''${path#$PERSIST_PATH/}"
          [[ -n "$REL_PATH" ]] && echo "  → $REL_PATH"
      done < "$MOUNTED_PATHS"
  fi

  # Clean up temp file
  rm -f "$MOUNTED_PATHS"
''
