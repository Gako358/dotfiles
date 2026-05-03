_: {
  flake.nixosModules.programs-nh =
    { pkgs, ... }:
    {
      programs.nh = {
        enable = true;
        clean = {
          enable = true;
          extraArgs = "--keep-since 4d --keep 3";
        };
        flake = "/home/merrinx/Sources/dotfiles";
      };

      environment.systemPackages = with pkgs; [
        nix-output-monitor
        nvd
      ];
    };
}
