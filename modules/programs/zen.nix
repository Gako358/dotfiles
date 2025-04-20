{ osConfig
, inputs
, config
, pkgs
, lib
, ...
}:
let
  # Create a wrapper script for zen-browser with Wayland enabled
  zenWithWayland = pkgs.symlinkJoin {
    name = "zen-browser-wayland";
    paths = [ inputs.zen-browser.packages."${pkgs.system}".default ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/zen \
        --set MOZ_ENABLE_WAYLAND 1
    '';
  };
in
{
  home = lib.mkIf osConfig.environment.desktop.enable {
    packages = [ zenWithWayland ];
    persistence."/persist/${config.home.homeDirectory}" = {
      directories = [
        ".zen"
      ];
    };
  };
}
