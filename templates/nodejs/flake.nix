{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
    in {
      devShell = let
        generateEditorConfig = pkgs.writeShellScriptBin "generateEditorConfig" ''
          if [ ! -f .editorconfig ]; then
            echo "root = true" > .editorconfig
            echo "" >> .editorconfig
            echo "[*]" >> .editorconfig
            echo "charset = utf-8" >> .editorconfig
            echo "end_of_line = lf" >> .editorconfig
            echo "indent_size = 2" >> .editorconfig
            echo "indent_style = space" >> .editorconfig
            echo "insert_final_newline = true" >> .editorconfig
            echo "max_line_length = 80" >> .editorconfig
            echo "trim_trailing_whitespace = true" >> .editorconfig
            echo "" >> .editorconfig
            echo "[*.md]" >> .editorconfig
            echo "max_line_length = 0" >> .editorconfig
            echo "trim_trailing_whitespace = false" >> .editorconfig
            echo "" >> .editorconfig
            echo "[COMMIT_EDITMSG]" >> .editorconfig
            echo "max_line_length = 0" >> .editorconfig
          fi
        '';
      in
        pkgs.mkShell {
          name = "java";
          buildInputs = with pkgs; [
            generateEditorConfig
            nodePackages.create-react-app
            nodejs
            yarn
          ];
          shellHook = ''
            ${generateEditorConfig}/bin/generateEditorConfig
          '';
        };
    });
}
