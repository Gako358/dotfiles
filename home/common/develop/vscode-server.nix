{
  lib,
  pkgs,
  config,
  ...
}:
with lib; let
  cfg = config.develop.vscode-server;
  codeServer = import(builtins.fetchTarball {
    url = "https://github.com/msteen/nixos-vscode-server/tarball/master"}/modules/vscode-server/home.nix";
    sha256 = "0dp2jakn0rpdvcsxzbpg37ifqh2lzcbdm2ycsqrs8sjdfrl7bj2m";
  });
in {
  options.develop.vscode-server = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable nix-ld by default if desktop env WSL";
    };
  };
  config = mkIf (cfg.enable && config.desktop.environment == "wsl") {
  services.vscode-server = {
    enable = true;
    nodejsPackage = pkgs.nodejs-17_x;
    installPath = "~/.vscode-server-insiders/";
    };
  };
}
