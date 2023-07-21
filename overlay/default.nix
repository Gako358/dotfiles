{inputs, ...}: let
  additions = final: _prev: import ../pkgs {pkgs = final;};
  modifications = final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });
  };
in
  inputs.nixpkgs.lib.composeManyExtensions [additions modifications]
