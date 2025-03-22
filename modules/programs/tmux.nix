{ pkgs, ... }:
let
  bg = "default";
  fg = "default";
  bg2 = "brightblack";
  fg2 = "white";
  color = c: "#{@${c}}";

  indicator = rec {
    accent = color "indicator_color";
    content = "  ";
    module = "#[reverse,fg=${accent}]#{?client_prefix,${content},}";
  };

  current_window = rec {
    accent = color "main_accent";
    index = "#[reverse,fg=${accent},bg=${fg}] #I ";
    name = "#[fg=${bg2},bg=${fg2}] #{b:pane_current_path} ";
    flags = "#{?window_flags,#{window_flags}, }";
    module = "${index}${name}";
  };

  window_status = rec {
    accent = color "window_color";
    index = "#[reverse,fg=${accent},bg=${fg}] #I ";
    name = "#[fg=${bg2},bg=${fg2}] #{b:pane_current_path} ";
    flags = "#{?window_flags,#{window_flags}, }";
    module = "${index}${name}";
  };
in
{
  programs.tmux = {
    enable = true;
    plugins = with pkgs.tmuxPlugins; [
      vim-tmux-navigator
      continuum
      resurrect
      yank
    ];
    prefix = "C-Space";
    baseIndex = 1;
    escapeTime = 0;
    keyMode = "vi";
    mouse = true;
    shell = "${pkgs.fish}/bin/fish";
    extraConfig = ''
      set -g @continuum-restore 'on'
      run-shell ${pkgs.tmuxPlugins.resurrect}/share/tmux-plugins/resurrect/resurrect.tmux
      run-shell ${pkgs.tmuxPlugins.continuum}/share/tmux-plugins/continuum/continuum.tmux
      set -g @resurrect-dir "~/.config/tmux/resurrect"
      set-option -sa terminal-overrides ",xterm*:Tc"

      # Set the status bar to be displayed at the top
      set-option -g status-position top

      # Use Alt-hjkl without prefix key to switch panes
      bind -n C-M-h select-pane -L
      bind -n C-M-l select-pane -R
      bind -n C-M-k select-pane -U
      bind -n C-M-j select-pane -D

      bind-key J resize-pane -D 5
      bind-key K resize-pane -U 5
      bind-key H resize-pane -L 5
      bind-key L resize-pane -R 5

      bind-key M-j resize-pane -D
      bind-key M-k resize-pane -U
      bind-key M-h resize-pane -L
      bind-key M-l resize-pane -R

      # Vim style pane selection
      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R

      # Use Alt-vim keys without prefix key to switch panes
      bind -n M-h select-pane -L
      bind -n M-j select-pane -D
      bind -n M-k select-pane -U
      bind -n M-l select-pane -R

      # Use Alt-arrow keys without prefix key to switch panes
      bind -n M-Left select-pane -L
      bind -n M-Right select-pane -R
      bind -n M-Up select-pane -U
      bind -n M-Down select-pane -D

      # Split windows
      bind-key v split-window -h
      bind-key x split-window

      bind-key M-p switch-client -l
      bind-key M-n switch-client -n

      bind f set-option -g status
      unbind -T copy-mode MouseDragEnd1Pane
      bind-key -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel "wl-copy"
      bind-key -T copy-mode-vi v send-keys -X begin-selection
      bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "wl-copy"
      bind-key -T copy-mode-vi r send-keys -X rectangle-toggle
      bind P paste-buffer

      set-option -g @indicator_color "yellow"
      set-option -g @window_color "magenta"
      set-option -g @main_accent "blue"
      set-option -g status-style "bg=${bg} fg=${fg}"
      set-option -g status-left "${indicator.module}"
      set-option -g status-right ""
      set-option -g window-status-current-format "${current_window.module}"
      set-option -g window-status-format "${window_status.module}"
      set-option -g window-status-separator ""
    '';
  };
}
