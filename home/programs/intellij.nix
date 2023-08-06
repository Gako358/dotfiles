{
  lib,
  pkgs,
  ...
}: let
  overrideWithGApps = pkg: pkg.overrideAttrs (oldAttrs: {nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [pkgs.wrapGAppsHook];});
  devSDKs = with pkgs; {
    java19 = jdk19;
    node = nodejs;
    yarn = yarn;
  };
  extraPath = lib.makeBinPath (builtins.attrValues devSDKs);
  idea-with-copilot = pkgs.jetbrains.plugins.addPlugins pkgs.jetbrains.idea-ultimate [
    "github-copilot"
  ];
  intellij =
    pkgs.runCommand "intellij"
    {nativeBuildInputs = [pkgs.makeWrapper];}
    ''
      mkdir -p $out/bin
      makeWrapper ${idea-with-copilot}/bin/idea-ultimate \
        $out/bin/intellij \
        --prefix PATH : ${extraPath}
    '';
  mkEntry = name: value: {
    inherit name;
    path = value;
  };
  entries = lib.mapAttrsToList mkEntry devSDKs;
  devSymlink = pkgs.linkFarm "local-dev" entries;
in {
  home.packages = [intellij];
  home.file.".local/dev".source = devSymlink;
}
