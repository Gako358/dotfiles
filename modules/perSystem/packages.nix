{ lib, ... }:
{
  perSystem =
    {
      pkgs,
      system,
      ...
    }:
    {
      packages = {
        repl = pkgs.callPackage ../../pkgs/repl { };
      }
      # raiderio-client and warcraftlogs are x86_64 AppImages, so they
      # don't evaluate on aarch64-linux. Only expose them where they work,
      # otherwise `nix flake check --all-systems` fails.
      // lib.optionalAttrs (system == "x86_64-linux") {
        raiderio-client = pkgs.callPackage ../../pkgs/raiderio-client { };
        warcraftlogs = pkgs.callPackage ../../pkgs/warcraftlogs { };
      };
    };
}
