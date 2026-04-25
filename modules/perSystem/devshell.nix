_: {
  perSystem =
    { pkgs, config, ... }:
    {
      devShells.default = pkgs.mkShell {
        name = "merrinx-dev-shell";
        inputsFrom = [ ];
        inherit (config.checks.pre-commit) shellHook;
        nativeBuildInputs = with pkgs; [
          nixfmt
        ];
      };
    };
}
