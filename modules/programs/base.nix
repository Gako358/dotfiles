_: {
  flake.nixosModules.programs-base =
    {
      pkgs,
      ...
    }:
    {
      # Core packages for system
      environment = {
        systemPackages = with pkgs; [
          wget
          git
        ];

        # Default editor for minor things
        variables.EDITOR = "nvim";
      };

      programs = {
        # Allow non-root users to specify the allow_other or allow_root mount options
        fuse.userAllowOther = true;
        # Nano is enabled by default, but not anymore...
        nano.enable = false;
        # NH is a modern helper utility that aims to consolidate and reimplement some of the commands from various tools within the NixOS ecosystem
        nh = {
          enable = true;
          clean.enable = true;
          clean.extraArgs = "--keep-since 4d --keep 3"; # Never keep more than 3
          flake = "/home/merrinx/Sources/dotfiles";
        };
      };
    };
}
