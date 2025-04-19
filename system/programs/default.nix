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
    ./fonts.nix
    ./qemu.nix
    ./shell.nix
    ./xdg.nix
  ];
}
