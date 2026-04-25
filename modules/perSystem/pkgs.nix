{ inputs, ... }:
{
  # flake-parts builds a default `pkgs` per system for use in perSystem modules
  # (devShells, formatter, packages, checks). That default is plain
  # `import nixpkgs { inherit system; }` with no `config` overrides, so it
  # rejects unfree packages — which breaks `nix flake check --all-systems`
  # for our `raiderio-client` and `warcraftlogs` AppImage wrappers.
  #
  # Override the default with one that mirrors the NixOS-level posture
  # (modules/hardware/base.nix sets nixpkgs.config.allowUnfree = true).
  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
      };
    };
}
