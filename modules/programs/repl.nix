{ pkgs
, self
, ...
}: {
  home.packages = [
    self.packages.${pkgs.system}.repl
  ];
}
