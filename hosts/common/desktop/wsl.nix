{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "wsl") {
    environment.systemPackages = with pkgs; [
      direnv
      nix-direnv
    ];

    # TODO: move to home-manager
    fonts.fontDir.enable = true;

    services.openssh = {
      enable = true;
      settings.passwordAuthentication = false;
    };
  };
}
