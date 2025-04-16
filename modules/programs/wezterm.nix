{ config, ... }: {
  programs.wezterm = {
    enable = true;

    extraConfig = /*lua*/ ''
      local wezterm = require "wezterm"

      -- wezterm.gui is not available to the mux server, so take care to
      -- do something reasonable when this config is evaluated by the mux
      wezterm.add_to_config_reload_watch_list(wezterm.config_dir)

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
            spawn = {
              cwd = "${config.home.homeDirectory}"
            },
          },
        },
        -- Switch to the dotfiles workspace
        {
          key = "d",
          mods = "LEADER",
          action = act.SwitchToWorkspace {
            name = "dotfiles",
            spawn = {
              cwd = "${config.home.homeDirectory}/Sources/dotfiles",
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
              cwd = "${config.home.homeDirectory}/Documents/notes",
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
              cwd = "${config.home.homeDirectory}/Projects/workspace",
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

      -- Tab bar configuration
      config.use_fancy_tab_bar = true
      config.show_new_tab_button_in_tab_bar = false
      config.tab_and_split_indices_are_zero_based = false
      config.tab_bar_at_bottom = true
      local LEFT_END = utf8.char(0xE0B6)
      local RIGHT_END = utf8.char(0xE0B4)
      local bg_color = "#1a1a1a"
      local active_tab_bg_color = "#FFA066"
      local inactive_text_color = "#719cd6"

      function tab_title(tab_info)
        local title = tab_info.tab_title
        -- if the tab title is explicitly set, take that
        if title and #title > 0 then
          return title
        end
        -- Otherwise, use the title from the active pane
        -- in that tab
        return tab_info.active_pane.title
      end

      wezterm.on(
        "format-tab-title",
        function(tab, tabs, panes, config, hover, max_width)
          local title = tab_title(tab)
          title = wezterm.truncate_right(title, max_width - 2)
          local tab_icon_active_icon = wezterm.nerdfonts.md_ghost
          local tab_icon_inactive_icon = wezterm.nerdfonts.md_ghost_off_outline
          local icon_text = " "
          local tab_icon_color = " "
          local tab_text_color = " "
          local tab_background_color = bg_color

          if tab.is_active then
            tab_icon_color = bg_color
            tab_text_color = bg_color
            tab_background_color = active_tab_bg_color
            icon_text = tab_icon_active_icon
          else
            tab_icon_color = inactive_text_color
            tab_text_color = inactive_text_color
            icon_text = tab_icon_inactive_icon
          end

          return {
            { Background = { Color = bg_color } },
            { Foreground = { Color = tab_background_color } },
            { Text = LEFT_END },
            { Background = { Color = tab_background_color } },
            { Foreground = { Color = tab_icon_color } },
            { Text = "  " .. icon_text .. "  " },
            { Background = { Color = tab_background_color } },
            { Foreground = { Color = tab_text_color } },
            { Text = title .. "   " },
            { Background = { Color = bg_color } },
            { Foreground = { Color = tab_background_color } },
            { Text = RIGHT_END },
          }
        end
      )

      -- Get the built-in nightfox scheme and modify it
      local nightfox_scheme = wezterm.get_builtin_color_schemes()["nightfox"]
      nightfox_scheme.background = "#1a1a1a"

      return {
        check_for_updates = false,
        color_schemes = {
          -- Override with our modified version
          ["Modified Nightfox"] = nightfox_scheme,
        },
        color_scheme = "Modified Nightfox",
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
        leader = config.leader,
        keys = config.keys,
        key_tables = config.key_tables,
        use_fancy_tab_bar = config.use_fancy_tab_bar,
        show_new_tab_button_in_tab_bar = config.show_new_tab_button_in_tab_bar,
        tab_and_split_indices_are_zero_based = config.tab_and_split_indices_are_zero_based,
      }
    '';
  };
}
