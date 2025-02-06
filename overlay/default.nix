{inputs, ...}: let
  additions = final: _prev: import ../pkgs {pkgs = final;};
  modifications = final: prev: {};
in
  inputs.nixpkgs.lib.composeManyExtensions [additions modifications]
