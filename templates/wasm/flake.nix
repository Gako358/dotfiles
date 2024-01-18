{
  description = "Rust wasm32 example";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , rust-overlay
    ,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ rust-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
        rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        inputs = [ rust pkgs.wasm-bindgen-cli pkgs.trunk ];
      in
      {
        defaultPackage = pkgs.rustPlatform.buildRustPackage {
          pname = "wasm-example";
          version = "0.1.0";
          src = ./.;
          cargoLock = {
            lockFile = ./Cargo.lock;
          };
          nativeBuildInputs = inputs;
          buildPhase = ''
            cargo build --release --target=wasm32-unknown-unknown
            echo 'Creating out dir...'
            mkdir -p $out/src;
            # Optional, of course
            # echo 'Copying package.json...'
            # cp ./package.json $out/;
            echo 'Generating node module...'
            wasm-bindgen \
              --target nodejs \
              --out-dir $out/src \
              target/wasm32-unknown-unknown/release/gcd.wasm;
          '';
          installPhase = ''
            "echo 'Skipping install phase...'";
          '';
        };

        devShell = pkgs.mkShell { packages = inputs; };
      }
    );
}
