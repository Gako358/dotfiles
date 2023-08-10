let
  ext = import ./extensions.nix;
in {
  programs.chromium = {
    enable = true;
    extensions = builtins.attrValues ext;
    commandLineArgs = ["--ozone-platform-hint=auto"];
  };
}
