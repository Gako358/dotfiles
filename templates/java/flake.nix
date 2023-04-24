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
      jdtls = pkgs.callPackage ./.jdtls.nix {};
    in {
      devShell = let
        generateEditorConfig = pkgs.writeShellScriptBin "generateEditorConfig" ''
          if [ ! -f .editorconfig ]; then
            echo "root = true" > .editorconfig
            echo "" >> .editorconfig
            echo "[*]" >> .editorconfig
            echo "end_of_line = lf" >> .editorconfig
            echo "insert_final_newline = true" >> .editorconfig
            echo "charset = utf-8" >> .editorconfig
            echo "" >> .editorconfig
            echo "# 4 space indentation" >> .editorconfig
            echo "[*.{java,html,js,xml,jsp}]" >> .editorconfig
            echo "indent_style = space" >> .editorconfig
            echo "tab_width = 4" >> .editorconfig
          fi
        '';
      in
        pkgs.mkShell {
          name = "java";
          buildInputs = with pkgs; [
            jdk
            jdtls
            generateEditorConfig
          ];
          shellHook = ''
            export JAVA_HOME=${pkgs.jdk}
            ${generateEditorConfig}/bin/generateEditorConfig
          '';
        };
    });
}
