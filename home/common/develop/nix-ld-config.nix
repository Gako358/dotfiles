{
  lib,
  pkgs,
  config,
  ...
}:
with lib; let
  cfg = config.develop.nix-ld-config;
  ldEnv = {
    NIX_LD_LIBRARY_PATH = with pkgs;
      makeLibraryPath [
        stdenv.cc.cc
      ];
    NIX_LD = removeSuffix "\n" (builtins.readFile "${pkgs.stdenv.cc}/nix-support/dynamic-linker");
  };
  ldExports = mapAttrsToList (name: value: "export ${name}=${value}") ldEnv;
  joinedLdExports = concatStringsSep "\n" ldExports;
in {
  options.develop.nix-ld-config = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable nix-ld by default if desktop env WSL";
    };
    user = mkOption {
      type = types.str;
      description = "The name of user you want to configure for using VSCode's Remote WSL extension.";
      default = "nixos";
    };
  };
  config = mkIf (cfg.enable && config.desktop.environment == "wsl") {
    environment.variables = ldEnv;
    home-manager.users.${cfg.user}.home.file.".vscode-server/server-env-setup".text = joinedLdExports;
  };
}
