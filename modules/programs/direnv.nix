{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };

  home.persistence."/persist/home/merrinx" = {
    directories = [
      ".local/share/direnv"
    ];
  };
}
