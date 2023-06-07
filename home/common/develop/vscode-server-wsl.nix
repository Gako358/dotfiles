{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with builtins; let
  patch = pkgs.fetchFromGitHub {
    # https://github.com/sonowz/vscode-remote-wsl-nixos
    owner = "sonowz";
    repo = "vscode-remote-wsl-nixos";
    rev = "e5dded6ee6e214fbf14a88f58775334ca5c19571";
    sha256 = "UcO4S0HqabTPu6wnB/fuWWdtKGFmf9fpWRuLUwGHo6o=";
  };
  cfg = config.develop.vscode-server-wsl;
in {
  options.develop.vscode-server-wsl = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable vscode-server-wsl";
    };
  };
  config = mkIf (cfg.enable && config.desktop.environment == "wsl") {
    home.file.".vscode-server-insiders/server-env-setup".source = "${patch}/flake/server-env-setup";
  };
}
