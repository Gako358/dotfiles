{ lib, ... }:
let
  moduleType = lib.mkOptionType {
    name = "module";
    description = "NixOS / home-manager module";
    check = x: lib.isAttrs x || lib.isFunction x || builtins.isPath x;
    merge = lib.mergeOneOption;
  };
in
{
  # `flake.nixosModules` is already declared by flake-parts itself, so we
  # rely on its built-in option here. Only `homeModules` needs a
  # custom declaration — flake-parts doesn't ship one for home-manager.
  options.flake.homeModules = lib.mkOption {
    type = lib.types.lazyAttrsOf moduleType;
    default = { };
    description = ''
      home-manager modules registered for composition by hosts.

      Each entry is consumed by every host's `home-manager.users.<user>`
      via `attrValues config.flake.homeModules`. Also surfaced as
      the conventional `homeModules` flake output for cross-flake
      consumption.
    '';
  };
}
