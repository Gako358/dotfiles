let
  more = {pkgs, ...}: {
    programs = {
      bat.enable = true;

      broot = {
        enable = true;
      };

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      fzf = {
        enable = true;
        defaultCommand = "fd --type file --follow"; # FZF_DEFAULT_COMMAND
        defaultOptions = ["--height 20%"]; # FZF_DEFAULT_OPTS
        fileWidgetCommand = "fd --type file --follow"; # FZF_CTRL_T_COMMAND
      };

      gpg.enable = true;
      jq.enable = true;

      ssh.enable = true;
    };
  };
in [
  ./browser/chromium.nix
  ./browser/discord.nix
  ./browser/firefox.nix
  ./browser/slack.nix
  ./config
  ./alacritty.nix
  #./citrix.nix
  ./git.nix
  ./jujutsu.nix
  ./network.nix
  ./zellij.nix
  more
]
