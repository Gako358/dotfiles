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
      };
    };
}
