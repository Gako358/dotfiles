{ pkgs
, ...
}:
{
  # Core pakages for system
  environment = {
    systemPackages = with pkgs; [
      wget
      curl
      git

      nodejs-18_x # Github Copilot requires nodejs 16
      alejandra # Nix formatting tool
    ];
    # Default editor for minor things
    variables.EDITOR = "nvim";
  };

  imports = [
    ./cachix
    ./dconf.nix
    ./docker.nix
    ./fish.nix
    ./fonts.nix
    ./qemu.nix
    ./xdg.nix
  ];

  programs = {
    # Allow non-root users to specify the allow_other or allow_root mount options
    fuse.userAllowOther = true;
    # Nano is enabled by default, but not anymore...
    nano.enable = false;
  };

}
