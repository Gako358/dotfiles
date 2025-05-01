_: {
  perSystem = { pkgs, ... }: {
    packages = {
      repl = pkgs.callPackage ./repl { };
    };
  };
}
