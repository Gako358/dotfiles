{
  programs.wezterm = {
    enable = true;

    extraConfig = /*lua*/''
      local wezterm = require "wezterm"

      -- wezterm.gui is not available to the mux server, so take care to
      -- do something reasonable when this config is evaluated by the mux
      function get_appearance()
        if wezterm.gui then
          return wezterm.gui.get_appearance()
        end
        return "Dark"
      end

      function scheme_for_appearance(appearance)
        if appearance:find "Dark" then
          return "nightfox"
        else
          return "dayfox"
        end
      end

      wezterm.add_to_config_reload_watch_list(wezterm.config_dir)
      wezterm.color_scheme_dirs = wezterm.color_scheme_dirs or {}

      -- Action shorthand
      local act = wezterm.action

      -- Show which key table is active in the status area
      wezterm.on("update-right-status", function(window, pane)
        local name = window:active_key_table()
        local workspace = window:active_workspace()
        local status = workspace
        if name then
          status = status .. " | TABLE: " .. name
        end
        window:set_right_status(status or "")
      end)

      -- Define our config
      config = wezterm.config_builder()

      -- Key bindings configuration
      config.leader = { key = "n", mods = "CTRL", timeout_milliseconds = 3000 }

      config.keys = {
        -- Splitting
        { mods = "LEADER", key = "-", action = act.SplitVertical { domain = "CurrentPaneDomain" } },
        { mods = "LEADER", key = "\\", action = act.SplitHorizontal { domain = "CurrentPaneDomain" } },

        -- Tab Navigation
        { mods = "LEADER", key = "1", action = act.ActivateTab(0) },
        { mods = "LEADER", key = "2", action = act.ActivateTab(1) },
        { mods = "LEADER", key = "3", action = act.ActivateTab(2) },
        { mods = "LEADER", key = "4", action = act.ActivateTab(3) },
        { mods = "LEADER", key = "5", action = act.ActivateTab(4) },
        { mods = "LEADER", key = "6", action = act.ActivateTab(5) },
        { mods = "LEADER", key = "7", action = act.ActivateTab(6) },
        { mods = "LEADER", key = "8", action = act.ActivateTab(7) },
        { mods = "LEADER", key = "9", action = act.ActivateTab(8) },

        -- Direct Pane Navigation
        { mods = "ALT", key = "h", action = act.ActivatePaneDirection("Left") },
        { mods = "ALT", key = "l", action = act.ActivatePaneDirection("Right") },
        { mods = "ALT", key = "k", action = act.ActivatePaneDirection("Up") },
        { mods = "ALT", key = "j", action = act.ActivatePaneDirection("Down") },
        { mods = "ALT", key = "x", action = act.CloseCurrentPane { confirm = false } },

        -- Mode activation
        {
          key = "r",
          mods = "LEADER",
          action = act.ActivateKeyTable {
            name = "resize_pane",
            one_shot = false,
          },
        },

        -- CTRL+n, followed by 'a' will put us in activate-pane mode
        {
          key = "a",
          mods = "LEADER",
          action = act.ActivateKeyTable {
            name = "activate_pane",
            timeout_milliseconds = 3000,
          },
        },

        -- Workspace management
        -- Switch to standard workspace
        {
          key = "s",
          mods = "LEADER",
          action = act.SwitchToWorkspace {
            name = "default",
          },
        },
        -- Switch to the dotfiles workspace
        {
          key = "d",
          mods = "LEADER",
          action = act.SwitchToWorkspace {
            name = "dotfiles",
            spawn = {
              cwd = "/home/merrinx/Sources/dotfiles",
            },
          },
        },
        -- Switch to ORG
        {
          key = "o",
          mods = "LEADER",
          action = act.SwitchToWorkspace {
            name = "org",
            spawn = {
              cwd = "/home/merrinx/Documents/notes",
            },
          },
        },
        -- Switch to a development workspace
        {
          key = "w",
          mods = "LEADER",
          action = act.SwitchToWorkspace {
            name = "work",
            spawn = {
              cwd = "/home/merrinx/Projects/workspace",
            },
          },
        },
      }

      config.key_tables = {
        -- Defines the keys that are active in resize-pane mode
        resize_pane = {
          { key = "h", action = act.AdjustPaneSize { "Left", 1 } },
          { key = "l", action = act.AdjustPaneSize { "Right", 1 } },
          { key = "k", action = act.AdjustPaneSize { "Up", 1 } },
          { key = "j", action = act.AdjustPaneSize { "Down", 1 } },

          -- Cancel the mode by pressing escape
          { key = "Escape", action = "PopKeyTable" },
        },

        -- Defines the keys that are active in activate-pane mode
        activate_pane = {
          { key = "h", action = act.ActivatePaneDirection "Left" },
          { key = "l", action = act.ActivatePaneDirection "Right" },
          { key = "k", action = act.ActivatePaneDirection "Up" },
          { key = "j", action = act.ActivatePaneDirection "Down" },
        },
      }

      -- Plugins
      wezterm.plugin
        .require('https://github.com/yriveiro/wezterm-status')
        .apply_to_config(config, {
            ui = {
              theme = {
                bg_color = '#88C0D0',
                fg_color = '#2E3440',
                intensity = 'Normal',
                underline = 'None',
                italic = true,
                strikethrough = false,
              },
              separators = {
                arrow_solid_left = '\u{e0b0}',
                arrow_solid_right = '\u{e0b2}',
                arrow_thin_left = '\u{e0b1}',
                arrow_thin_right = '\u{e0b3}',
              }
            },
            cells = {
              battery = { enabled = false },
              hostname = { enabled = false },
              date = { format = '%H:%M' },
              mode = {
                enabled = true,
                modes = {
                  normal = ' ' .. wezterm.nerdfonts.cod_home,
                  copy_mode = ' ' .. wezterm.nerdfonts.cod_copy,
                  search_mode = ' ' .. wezterm.nerdfonts.cod_search,
                }
              },
              cwd = {
                enabled = true,
                tilde_prefix = true,
                path_aliases = {
                  { pattern = os.getenv("HOME") .. "/Projects", replacement = "󰲋 " },    -- Projects folder
                  { pattern = os.getenv("HOME") .. "/Projects/workspace", replacement = "󱌣 " },  -- Workspace folder
                  { pattern = os.getenv("HOME") .. "/Documents", replacement = "󰈙 " },   -- Documents folder
                  { pattern = os.getenv("HOME") .. "/Sources/dotfiles", replacement = "󱄯 " }    -- Dotfiles folder
                }
              }
            }
          })

      return {
        check_for_updates = false,
        color_scheme = "nightfox",
        default_cursor_style = "SteadyBar",
        enable_scroll_bar = true,
        font = wezterm.font_with_fallback({
          {family = "FiraCode", weight = "Medium"},
          {family = "JetBrainsMono Nerd Font", italic = true},
        }),
        font_size = 10,
        hide_tab_bar_if_only_one_tab = true,
        scrollback_lines = 10000,
        window_background_opacity = 0.9,
        colors = {
          background = "#1a1a1a",
        },
        leader = config.leader,
        keys = config.keys,
        key_tables = config.key_tables,

        -- Include any plugin-related configurations
        tab_bar_style = config.tab_bar_style,
        status_update_interval = config.status_update_interval,
        wezterm_plugin_bar = config.wezterm_plugin_bar,
      }
    '';
  };
}
