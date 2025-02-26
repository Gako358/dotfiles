let
  scripts = {pkgs, ...}: let
    countdown-timer = pkgs.callPackage ./countdown-timer.nix {inherit pkgs;};
    gen-ssh-key = pkgs.callPackage ./gen-ssh-key.nix {inherit pkgs;};
    set-monitor = pkgs.callPackage ./set-monitor.nix {inherit pkgs;};
    handle-monitor = pkgs.callPackage ./handle-monitor.nix {inherit pkgs;};
  in {
    home.packages =
      [
        countdown-timer # countdown timer with figlet
        gen-ssh-key # generate ssh key and add it to the system
        set-monitor # set monitor resolution
        handle-monitor # handle monitor resolution
        set-monitor # set monitor resolution
      ]
      ++ (pkgs.sxm.scripts or []);
  };
in [scripts]
