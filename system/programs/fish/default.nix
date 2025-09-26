{ pkgs
, lib
, ...
}:
let
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

  sharedAliases = import ./fish-aliases.nix { inherit pkgs lib; };
in
{
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
    shellAliases = sharedAliases.fishAliases;
    shellInit = fishConfig;
  };
}
