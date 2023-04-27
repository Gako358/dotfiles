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
      javaVersion = 8;

      overlays = [
        (self: super: rec {
          jdk = super."jdk${toString javaVersion}";
          maven = super.maven.override {
            inherit jdk;
          };
        })
      ];
      pkgs = import nixpkgs {inherit overlays system;};
    in {
      devShell = let
        generateEditorConfig = pkgs.writeShellScriptBin "generateEditorConfig" ''
          if [ ! -f .editorconfig ]; then
            echo "root = true" > .editorconfig
            echo "" >> .editorconfig
            echo "[*]" >> .editorconfig
            echo "end_of_line = lf" >> .editorconfig
            echo "insert_final_newline = true" >> .editorconfig
            echo "" >> .editorconfig
            echo "# 4 space indentation" >> .editorconfig
            echo "[*.{java,html,js,xml,jsp}]" >> .editorconfig
            echo "indent_style = space" >> .editorconfig
            echo "tab_width = 4" >> .editorconfig
          fi
        '';
        mvnRunAll = pkgs.writeShellScriptBin "mvnRunAll" ''
          mvn clean package -PsetLocal,runAllDb,importAll
        '';
        mvnP = pkgs.writeShellScriptBin "mvnP" ''
          mvn package
        '';
        mvnCP = pkgs.writeShellScriptBin "mvnCP" ''
          mvn clean package
        '';
      in
        pkgs.mkShell {
          name = "java";
          buildInputs = with pkgs; [
            # ${generateEditorConfig}/bin/generateEditorConfig
            jdk
            maven
            generateEditorConfig
            mvnRunAll
            mvnCP
            mvnP
          ];
          shellHook = ''
            export JAVA_HOME=${pkgs.jdk}
          '';
        };
    });
}
