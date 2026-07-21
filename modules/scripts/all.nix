_: {
  flake.homeModules.scripts =
    { pkgs, ... }:
    let
      build-installer = pkgs.callPackage ./_build-installer.nix { inherit pkgs; };
      countdown-timer = pkgs.callPackage ./_countdown-timer.nix { inherit pkgs; };
      gen-ssh-key = pkgs.callPackage ./_gen-ssh-key.nix { inherit pkgs; };
      orphant-persist = pkgs.callPackage ./_orphant-persist.nix { inherit pkgs; };
      set-monitor = pkgs.callPackage ./_set-monitor.nix { inherit pkgs; };
      handle-monitor = pkgs.callPackage ./_handle-monitor.nix { inherit pkgs; };
      convert-scala-utf8 = pkgs.callPackage ./_convert-scala-utf8.nix { inherit pkgs; };
      git-lines-total = pkgs.callPackage ./_git-lines-total.nix { inherit pkgs; };
      git-lines-by-repo = pkgs.callPackage ./_git-lines-by-repo.nix { inherit pkgs; };
    in
    {
      home.packages = [
        build-installer
        countdown-timer
        gen-ssh-key
        orphant-persist
        set-monitor
        handle-monitor
        convert-scala-utf8
        git-lines-total
        git-lines-by-repo
      ]
      ++ (pkgs.sxm.scripts or [ ]);
    };
}
