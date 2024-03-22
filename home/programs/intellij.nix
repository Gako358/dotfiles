{pkgs, ...}: let
  overrideWithGApps = pkg: pkg.overrideAttrs (oldAttrs: {nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [pkgs.wrapGAppsHook];});
  idea-with-copilot = pkgs.jetbrains.plugins.addPlugins pkgs.jetbrains.idea-ultimate [
    "github-copilot"
  ];
  intellij =
    pkgs.runCommand "intellij"
    {nativeBuildInputs = [pkgs.makeWrapper];}
    ''
      mkdir -p $out/bin
      makeWrapper ${idea-with-copilot}/bin/idea-ultimate \
        $out/bin/intellij
    '';
in {
  home.packages = [intellij];
}
