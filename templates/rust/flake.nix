{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    naersk,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
      naersk-lib = pkgs.callPackage naersk {};
      # Package set for this system, add packages here
    in {
      devShell = let
        generateEditorConfig = pkgs.writeShellSciptBin "generateEditorConfig" ''
          if [ ! -f .editorconfig ]; then
            echo "root = true" > .editorconfig
            echo "" >> .editorconfig
            echo "[*]" >> .editorconfig
            echo "end_of_line = lf" >> .editorconfig
            echo "charset = utf-8" >> .editorconfig
            echo "trim_trailing_whitespace = true" >> .editorconfig
            echo "insert_final_newline = true" >> .editorconfig
            echo "indent_style = space" >> .editorconfig
            echo "indent_size = 4" >> .editorconfig
            echo "max_line_length = 120" >> .editorconfig
            echo "" >> .editorconfig
            echo "[*.md]" >> .editorconfig
            echo "trim_trailing_whitespace = false" >> .editorconfig
            echo "" >> .editorconfig
            echo "[*.yml]" >> .editorconfig
            echo "indent_size = 2" >> .editorconfig
          fi
        '';
      in
        pkgs.mkShell {
          name = "Rust devShell";
          buildInputs = with pkgs; [
            cargo
            rustc
            rustfmt
            pre-commit
            rustPackages.clippy
          ];
          shellHook = ''
            ${generateEditorConfig}/bin/generateEditorConfig
          '';
        };
    });
}
