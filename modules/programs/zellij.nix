{ config, ... }:
let
  defaultLayout = ''
    layout {
      default_tab_template {
        pane size=1 borderless=true {
            plugin location="compact-bar"
        }
        children
      }
      tab name="code"
      tab name="build" {
        pane split_direction="vertical" {
          pane
          pane
        }
      }
    }
  '';

  registerLayout = ''
    layout {
      default_tab_template {
        pane size=1 borderless=true {
            plugin location="compact-bar"
        }
        children
      }
      tab name="shell" {
        pane
        pane split_direction="vertical" {
          pane
          pane
        }
      }
      tab name="code"
      tab name="logs" {
        pane split_direction="vertical" {
          pane
          pane
        }
      }
    }
  '';

  zellijConfig = ''
    keybinds clear_deaults=true {
      normal {
        unbind "Ctrl a"
        unbind "Ctrl b"
        unbind "Ctrl h"
        unbind "Ctrl m"
        unbind "Ctrl o"
        unbind "Ctrl p"
        unbind "Ctrl q"
        unbind "Ctrl s"
        unbind "Ctrl t"
        unbind "Ctrl w"
      }
      tmux clear-defaults=true {
        bind "Esc" { SwitchToMode "Normal"; }

        bind "l" { SwitchToMode "Locked"; }
        bind "p" { SwitchToMode "Pane"; }
        bind "t" { SwitchToMode "Tab"; }
        bind "n" { SwitchToMode "Resize"; }
        bind "o" { SwitchToMode "Session"; }
        bind "/" { SwitchToMode "Search"; }
        bind "q" { Quit; }

        bind "f" { ToggleFocusFullscreen; SwitchToMode "Normal"; }
        bind "s" { ToggleFloatingPanes; SwitchToMode "Normal"; }

        bind "1" { GoToTab 1; SwitchToMode "Normal"; }
        bind "2" { GoToTab 2; SwitchToMode "Normal"; }
        bind "3" { GoToTab 3; SwitchToMode "Normal"; }
        bind "4" { GoToTab 4; SwitchToMode "Normal"; }
        bind "5" { GoToTab 5; SwitchToMode "Normal"; }
        bind "6" { GoToTab 6; SwitchToMode "Normal"; }
        bind "7" { GoToTab 7; SwitchToMode "Normal"; }
        bind "8" { GoToTab 8; SwitchToMode "Normal"; }
        bind "9" { GoToTab 9; SwitchToMode "Normal"; }
      }
      shared_except "locked" {
        bind "Alt n" { GoToPreviousTab; SwitchToMode "Normal"; }
        bind "Alt p" { GoToNextTab; SwitchToMode "Normal"; }
        bind "Alt h" "Alt Left" { MoveFocusOrTab "Left"; }
        bind "Alt l" "Alt Right" { MoveFocusOrTab "Right"; }
        bind "Alt f" "Alt Down" { MoveFocus "Down"; }
        bind "Alt k" "Alt Up" { MoveFocus "Up"; }

        bind "Ctrl x" { CloseFocus; SwitchToMode "Normal"; }
      }
      shared_except "tmux" "locked" {
        bind "Ctrl n" { SwitchToMode "Tmux"; }
      }
    }
    theme "customTheme"
    themes {
      customTheme {
        bg "#2b3b51"
        fg "#cdcecf"
        red "#c94f6d"
        green "#719cd6"
        blue "#719cd6"
        yellow "#dbc074"
        magenta "#9d79d6"
        orange "#f4a261"
        cyan "#63cdcf"
        black "#1a1a1a"
        white "#aeafb0"
      }
    }
    on_force_close "quit"
    pane_frames false
    default_layout "default_layout"
    default_mode "normal"
    default_shell "fish"
    scrollback_editor "hx"
    layout_dir "${config.xdg.configHome}/zellij/layouts"
  '';
in
{
  programs.zellij = {
    enable = true;
    enableFishIntegration = false;
  };

  home.file."./.config/zellij/layouts/default_layout.kdl".text = defaultLayout;
  home.file."./.config/zellij/layouts/register_layout.kdl".text = registerLayout;
  home.file."./.config/zellij/config.kdl".text = zellijConfig;
}
