{
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  cfg = config.desktop;
in {
  config = mkIf (cfg.environment == "dwm" || cfg.environment == "bspwm") {
    # Set the default editor to nvim
    environment.variables = {
      "EDITOR" = "nvim";
      "VISUAL" = "nvim";
    };
    # Turn off screen blanking
    environment.extraInit = ''
      xset s off
      xset -dpms
      xset s noblank
    '';
  };
}
