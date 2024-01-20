{pkgs, ...}: let
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
    name = "#[fg=${bg2},bg=${fg2}] #W ";
    flags = "#{?window_flags,#{window_flags}, }";
    module = "${index}${name}";
  };

  window_status = rec {
    accent = color "window_color";
    index = "#[reverse,fg=${accent},bg=${fg}] #I ";
    name = "#[fg=${bg2},bg=${fg2}] #W ";
    flags = "#{?window_flags,#{window_flags}, }";
    module = "${index}${name}";
  };

  time = rec {
    accent = color "main_accent";
    format = "%H:%M";

    icon =
      pkgs.writeShellScriptBin "icon" ''
        hour=$(date +%H)
        if   [ "$hour" == "00" ] || [ "$hour" == "12" ]; then printf "󱑖"
        elif [ "$hour" == "01" ] || [ "$hour" == "13" ]; then printf "󱑋"
        elif [ "$hour" == "02" ] || [ "$hour" == "14" ]; then printf "󱑌"
        elif [ "$hour" == "03" ] || [ "$hour" == "15" ]; then printf "󱑍"
        elif [ "$hour" == "04" ] || [ "$hour" == "16" ]; then printf "󱑎"
        elif [ "$hour" == "05" ] || [ "$hour" == "17" ]; then printf "󱑏"
        elif [ "$hour" == "06" ] || [ "$hour" == "18" ]; then printf "󱑐"
        elif [ "$hour" == "07" ] || [ "$hour" == "19" ]; then printf "󱑑"
        elif [ "$hour" == "08" ] || [ "$hour" == "20" ]; then printf "󱑒"
        elif [ "$hour" == "09" ] || [ "$hour" == "21" ]; then printf "󱑓"
        elif [ "$hour" == "10" ] || [ "$hour" == "22" ]; then printf "󱑔"
        elif [ "$hour" == "11" ] || [ "$hour" == "23" ]; then printf "󱑕"
        fi
      ''
      + "/bin/icon";

    module = "#[reverse,fg=${accent}] ${format} #(${icon}) ";
  };

  bat = rec {
    percentage =
      pkgs.writeShellScriptBin "percentage" ''
        path="/org/freedesktop/UPower/devices/DisplayDevice"
        percentage=$(${pkgs.upower}/bin/upower -i $path | grep percentage | awk '{print $2}' | tr '%' ' ')
        echo $percentage
      ''
      + "/bin/percentage";

    state =
      pkgs.writeShellScriptBin "state" ''
        path="/org/freedesktop/UPower/devices/DisplayDevice"
        state=$(${pkgs.upower}/bin/upower -i $path | grep state | awk '{print $2}')
        echo $state
      ''
      + "/bin/state";

    icon =
      pkgs.writeShellScriptBin "icon" ''
        percentage=$(${percentage})
        state=$(${state})
        if [ "$state" == "charging" ] || [ "$state" == "fully-charged" ]; then
            echo "󰂄"
        elif [ $percentage -ge 75 ]; then printf "󱊣"
        elif [ $percentage -ge 50 ]; then printf "󱊢"
        elif [ $percentage -ge 25 ]; then printf "󱊡"
        elif [ $percentage -ge 0  ]; then printf "󰂎"
        fi
      ''
      + "/bin/icon";

    color =
      pkgs.writeShellScriptBin "color" ''
        percentage=$(${percentage})
        state=$(${state})
        if [ "$state" == "charging" ] || [ "$state" == "fully-charged" ]; then
            echo "green"
        elif [ $percentage -ge 75 ]; then printf "green"
        elif [ $percentage -ge 50 ]; then printf "${fg2}"
        elif [ $percentage -ge 25 ]; then printf "yellow"
        elif [ $percentage -ge 0  ]; then printf "red"
        fi
      ''
      + "/bin/color";

    module = "#[fg=#(${color})]#(${icon}) #[fg=${fg}]#(${percentage})%";
  };

  pwd = rec {
    accent = color "main_accent";
    icon = "#[fg=${accent}] ";
    format = "#[fg=${fg}]#{b:pane_current_path}";
    module = "${icon}${format}";
  };
in {
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

      # Shift arrow to switch windows
      bind -n S-Left  previous-window
      bind -n S-Right next-window

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
      set-option -g status-right "${pwd.module} | ${bat.module} ${time.module}"
      set-option -g window-status-current-format "${current_window.module}"
      set-option -g window-status-format "${window_status.module}"
      set-option -g window-status-separator ""
    '';
  };
}
