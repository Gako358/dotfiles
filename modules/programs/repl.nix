{ pkgs
, self
, ...
}:
{
  home.packages = [
    self.packages.${pkgs.stdenv.hostPlatform.system}.repl
  ];
}
