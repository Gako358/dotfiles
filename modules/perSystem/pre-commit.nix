{ inputs, ... }:
{
  perSystem =
    { system, ... }:
    {
      checks.pre-commit = inputs.pre-commit-hooks-nix.lib.${system}.run {
        src = ../..;
        hooks = {
          statix.enable = true;
          deadnix.enable = true;
          nil.enable = true;
          nixfmt.enable = true;
          shellcheck.enable = true;
          beautysh.enable = true;
        };
      };
    };
}
