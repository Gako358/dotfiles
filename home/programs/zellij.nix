let
  defaultLayout = ''
    layout {
      pane size=1 borderless=true {
          plugin location="compact-bar"
      }
      pane
    }
  '';

  projectLayout = ''
    layout {
      tab name="shell" {
        pane size=1 borderless=true {
            plugin location="compact-bar"
        }
        pane
        pane split_direction="vertical" {
          pane
          pane
        }
      }
      tab name="code" {
        pane size=1 borderless=true {
            plugin location="compact-bar"
        }
        pane
        floating_panes {
          pane {
            width "50%"
            height "50%"
          }
        }
      }
      tab name="debug" {
        pane size=1 borderless=true {
            plugin location="compact-bar"
        }
        pane split_direction="vertical" {
          pane
          pane
        }
      }
    }
  '';

  zellijConfig = ''
    theme "myNightfox"
    themes {
      myNightfox {
        bg "#2b3b51"
        fg "#cdcecf"
        red "#c94f6d"
        green "#719cd6"
        blue "#719cd6"
        yellow "#dbc074"
        magenta "#9d79d6"
        orange "#f4a261"
        cyan "#63cdcf"
        black "#282828"
        white "#aeafb0"
      }
    }
    on_force_close "quit"
    pane_frames false
    default_layout "project_layout"
    default_mode "normal"
    default_shell "fish"
    pane_frames false
    scrollback_editor "hx"
  '';
in {
  programs.zellij = {
    enable = true;
    enableFishIntegration = true;
  };

  home.file."./.config/zellij/layouts/default_layout.kdl".text = defaultLayout;
  home.file."./.config/zellij/layouts/project_layout.kdl".text = projectLayout;
  home.file."./.config/zellij/config.kdl".text = zellijConfig;
}
