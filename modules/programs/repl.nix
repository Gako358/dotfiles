_: {
  flake.homeModules.programs-repl =
    {
      pkgs,
      self,
      ...
    }:
    {
      home.packages = [
        self.packages.${pkgs.stdenv.hostPlatform.system}.repl
      ];
    };
}
