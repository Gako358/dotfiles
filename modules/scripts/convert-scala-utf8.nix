{ pkgs, ... }:
let
  file = "${pkgs.file}/bin/file";
  grep = "${pkgs.gnugrep}/bin/grep";
  cut = "${pkgs.coreutils}/bin/cut";
  iconv = "${pkgs.libiconv}/bin/iconv";
  sed = "${pkgs.gnused}/bin/sed";
  find = "${pkgs.findutils}/bin/find";
  echo = "${pkgs.coreutils}/bin/echo";
  cp = "${pkgs.coreutils}/bin/cp";
  mv = "${pkgs.coreutils}/bin/mv";
  rm = "${pkgs.coreutils}/bin/rm";
in
pkgs.writeShellScriptBin "convert-scala-utf8" ''
  SCALA_FILES=$(${find} . -name "*.scala" -type f)
  CONVERTED=0
  FAILED=0
  SKIPPED=0

  if [ -z "$SCALA_FILES" ]; then
    ${echo} "No .scala files found"
    exit 1
  fi

  ${echo} "Found $(${echo} "$SCALA_FILES" | ${grep} -c "")" .scala files""
  ${echo} ""

  while IFS= read -r file; do
    if [ ! -f "$file" ]; then
      continue
    fi

    # Skip generated files in target/
    if [[ "$file" == "./target/"* ]]; then
      ${echo} "⊘ Skipping generated file: $file"
      ((SKIPPED++))
      continue
    fi

    current_charset=$(${file} -i "$file" | ${grep} -o "charset=[^ ]*" | ${cut} -d= -f2)

    ${echo} "Converting: $file (current: $current_charset)"

    # Create backup
    ${cp} "$file" "$file.bak"

    # Convert from current charset to UTF-8
    if [ "$current_charset" = "us-ascii" ]; then
      # US-ASCII can be directly treated as UTF-8
      ${iconv} -f US-ASCII -t UTF-8 "$file" > "$file.tmp" 2>/dev/null
    else
      # For UTF-8 or unknown, try UTF-8 with character removal
      ${iconv} -f UTF-8 -t UTF-8 -c "$file" > "$file.tmp" 2>/dev/null
    fi

    if [ $? -eq 0 ]; then
      ${mv} "$file.tmp" "$file"

      # Remove BOM if it exists
      ${sed} -i "1s/^\xef\xbb\xbf//" "$file"

      # Verify the file is valid UTF-8
      if ${file} -i "$file" | ${grep} -q "utf-8\|us-ascii"; then
        ${echo} "  ✓ Successfully converted to UTF-8"
        ${rm} "$file.bak"
        ((CONVERTED++))
      else
        ${echo} "  ✗ Conversion verification failed, restoring backup"
        ${mv} "$file.bak" "$file"
        ((FAILED++))
      fi
    else
      ${echo} "  ✗ Error during conversion, restoring backup"
      ${mv} "$file.bak" "$file"
      ((FAILED++))
    fi

  done <<< "$SCALA_FILES"

  ${echo} ""
  ${echo} "========================================="
  ${echo} "Summary:"
  ${echo} "  Converted: $CONVERTED"
  ${echo} "  Failed: $FAILED"
  ${echo} "  Skipped (generated): $SKIPPED"
  ${echo} "========================================="
''
