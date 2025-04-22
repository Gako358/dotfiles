{ inputs, lib, config, pkgs, system, ... }: {
  # Use the standard perSystem arguments, including 'pkgs'
  perSystem = { pkgs, lib, config, system, ... }: {
    packages = {
      # Use the 'pkgs' provided to perSystem
      pass-wofi = pkgs.callPackage ./pass-wofi {
        # No overrides needed here if pass-wofi/default.nix defines all deps
      };
    };
  };
}
