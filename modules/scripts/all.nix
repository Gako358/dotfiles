_: {
  flake.homeModules.scripts =
    { pkgs, ... }:
    let
      countdown-timer = pkgs.callPackage ./_countdown-timer.nix { inherit pkgs; };
      gen-ssh-key = pkgs.callPackage ./_gen-ssh-key.nix { inherit pkgs; };
      orphant-persist = pkgs.callPackage ./_orphant-persist.nix { inherit pkgs; };
      set-monitor = pkgs.callPackage ./_set-monitor.nix { inherit pkgs; };
      handle-monitor = pkgs.callPackage ./_handle-monitor.nix { inherit pkgs; };
      convert-scala-utf8 = pkgs.callPackage ./_convert-scala-utf8.nix { inherit pkgs; };
    in
    {
      home.packages = [
        countdown-timer
        gen-ssh-key
        orphant-persist
        set-monitor
        handle-monitor
        convert-scala-utf8
      ]
      ++ (pkgs.sxm.scripts or [ ]);
    };
}
