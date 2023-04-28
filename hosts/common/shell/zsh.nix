{
  pkgs,
  config,
  lib,
  ...
}:
with builtins;
with lib; let
  shellAliases = {
    ll = "ls -la";
    df = "df -Th";

    gc = "nix-collect-garbage";
    gcd = "sudo nix-collect-garbage -d";

    # Locations
    dot = "cd ~/Sources/dotfiles";
    doc = "cd ~/Documents/Reports";
    nvim = "cd ~/Projects/neovim";
    work = "cd ~/Projects/";
    www = "cd ~/Projects/wwwsite/";

    # Git
    ga = "git add -A && git commit";
    gp = "git push -u origin main";
    gs = "git status";
    gt = "git status -uno";
    gf = "git fetch --dry-run";
    gd = "git diff";

    update = "nix flake update";
    supdate = "sudo nix flake update";
    upgrade = "sudo nixos-rebuild switch --flake";

    homeflake = "home-manager switch --flake";
    homeflake_install = "nix run github:nix-community/home-manager#home-manager -- switch --flake";

    rzsh = "source ~/.zshrc";
  };
in {
  environment.systemPackages = with pkgs; [
    spaceship-prompt
    nix-zsh-completions
    fzf
    fzf-zsh
    broot
    jump
  ];

  users.defaultUserShell = pkgs.zsh;

  programs.zsh = {
    inherit shellAliases;
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;
    enableBashCompletion = true;
    interactiveShellInit = ''
      eval "$(direnv hook zsh)"
      eval "$(jump shell)"
    '';
    shellInit = ''
      source ${pkgs.spaceship-prompt}/share/zsh/site-functions/prompt_spaceship_setup

      # Simple keybindings for moving around commands in history.
      bindkey -e
      bindkey "$terminfo[khome]" beginning-of-line # Home
      bindkey "$terminfo[kend]" end-of-line # End
      bindkey "$terminfo[kich1]" overwrite-mode # Insert
      bindkey "$terminfo[kdch1]" delete-char # Delete
      bindkey "$terminfo[kcub1]" backward-char # Left
      bindkey "$terminfo[kcuf1]" forward-char # Right

      # case insensitive
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
    '';

    promptInit = ''
      autoload -U promptinit; promptinit
      prompt spaceship
      # Make history searchable
      autoload -U up-line-or-beginning-search
      autoload -U down-line-or-beginning-search
      zle -N up-line-or-beginning-search
      zle -N down-line-or-beginning-search
      bindkey "$key[Up]" up-line-or-beginning-search
      bindkey "$key[Down]" down-line-or-beginning-search
    '';

    setOptions = [
      "autocd"
      "append_history"
      "hist_ignore_dups"
      "hist_ignore_space"
      "EXTENDED_HISTORY"
    ];
  };
}
