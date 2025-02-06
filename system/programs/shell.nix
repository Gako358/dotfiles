{
  pkgs,
  lib,
  ...
}: let
  fzfConfig = ''
    set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'" \n
    set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."
  '';

  themeConfig = ''
    set -g theme_display_date no
    set -g theme_display_git_master_branch no
    set -g theme_nerd_fonts yes
    set -g theme_newline_cursor yes
    set -g theme_color_scheme solarized
  '';

  fishConfig =
    ''
      set fish_greeting
    ''
    + fzfConfig
    + themeConfig;

  dc = "${pkgs.docker-compose}/bin/docker-compose";
in {
  programs.fish = {
    enable = true;
    vendor = {
      completions.enable = true;
      config.enable = true;
      functions.enable = true;
    };
    interactiveShellInit = ''
      eval (direnv hook fish)
      jump shell fish | source
      any-nix-shell fish --info-right | source
    '';
    shellAliases = {
      inherit dc;
      cat = "bat";
      dps = "${dc} ps";
      dcd = "${dc} down --remove-orphans";
      drm = "docker images -a -q | xargs docker rmi -f";
      du = "${pkgs.ncdu}/bin/ncdu --color dark -rr -x";
      ls = "${pkgs.eza}/bin/eza";
      la = "${lib.getExe pkgs.eza} --long --all --group --header --group-directories-first --sort=type --icons";
      lg = "${lib.getExe pkgs.eza} --long --all --group --header --git";
      lt = "${lib.getExe pkgs.eza} --long --all --group --header --tree --level ";

      ".." = "cd ..";

      zz = "zellij";
      ping = "${pkgs.prettyping}/bin/prettyping";
      tree = "${pkgs.eza}/bin/eza -T";
      xdg-open = "${pkgs.mimeo}/bin/mimeo";

      # Nix
      gc = "nix-collect-garbage";
      gcd = "sudo nix-collect-garbage -d";

      # Locations
      dot = "cd ~/Sources/dotfiles";
      doc = "cd ~/Documents";
      neovim = "cd ~/Projects/neovim";
      work = "cd ~/Projects/workspace/";
      www = "cd ~/Projects/wwwsite/";

      update = "nix flake update";
      supdate = "sudo nix flake update";
      upgrade = "sudo nixos-rebuild switch --flake";

      homeflake = "home-manager switch --flake";
      homeflake_install = "nix run github:nix-community/home-manager#home-manager -- switch --flake";
    };
    shellInit = fishConfig;
  };
}
