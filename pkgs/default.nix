{ inputs, lib, config, pkgs, system, ... }: {
  perSystem = { pkgs, lib, config, system, ... }: {
    packages = {
      pass-wofi = pkgs.callPackage ./pass-wofi { };
    };
  };
}
