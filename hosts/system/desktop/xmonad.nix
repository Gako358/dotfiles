{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "xmonad") {
    services.xserver = {
      enable = true;
      layout = "us";
      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
      };
      serverLayoutSection = ''
        Option "StandbyTime" "0"
        Option "SuspendTime" "0"
        Option "OffTime"     "0"
      '';
      xkbVariant = "";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      displayManager = {
        lightdm = {
          enable = true;
          greeters.enso.enable = true;
          background = pkgs.nixos-artwork.wallpapers.nineish-dark-gray.gnomeFilePath;
        };
        defaultSession = "none+xmonad";
      };

      # Exlude the following packages from the Xorg server
      excludePackages = [
        pkgs.nano
        pkgs.xterm
      ];
    };
    # Set fish as the default shell
    programs.fish.enable = true;
    # Enable gnome-keyring in PAM
    security.pam.services.lightdm.enableGnomeKeyring = true;
  };
}
