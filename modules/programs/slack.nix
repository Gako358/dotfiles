{ osConfig
, pkgs
, lib
, ...
}:
let
  inherit (osConfig.environment) desktop;
in
{
  home = lib.mkIf (desktop.enable && desktop.develop) {
    packages = [
      pkgs.slack
    ];
    persistence."/persist/" = {
      directories = [
        ".config/Slack"
      ];
    };
  };
}
