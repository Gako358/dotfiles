{ inputs, ... }:
{
  flake.nixosModules.programs-hyprland =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    {
      imports = [ inputs.hyprland.nixosModules.default ];

      config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
        environment = {
          systemPackages = [
            inputs.hyprland-contrib.packages.${pkgs.stdenv.hostPlatform.system}.grimblast
          ];
          pathsToLink = [ "/share/icons" ];
          variables.NIXOS_OZONE_WL = "1";
        };

        programs.hyprland = {
          enable = true;
          withUWSM = true;
          package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
          portalPackage =
            inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
          plugins = with inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}; [
            # hyprbars
            # hyprexpo
          ];
        };

        xdg.portal = {
          enable = true;
          xdgOpenUsePortal = true;
          config = {
            common.default = [ "gtk" ];
            hyprland.default = [
              "gtk"
              "hyprland"
            ];
          };
          extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
        };
      };
    };

  flake.homeModules.programs-hyprland =
    {
      config,
      lib,
      pkgs,
      osConfig,
      ...
    }:
    let
      inherit (config.colorScheme) palette;

      hyprSettings = osConfig.programs.hyprland.settings or { };
      monitorList = hyprSettings.monitor or [ ];
      workspaceList = hyprSettings.workspace or [ ];

      escapeLua = s: builtins.replaceStrings [ "\\" "\"" ] [ "\\\\" "\\\"" ] s;
      toLuaStr = s: "\"${escapeLua s}\"";
      toLuaList = items: "{ " + (lib.concatMapStringsSep ", " toLuaStr items) + " }";
    in
    {
      config = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
        xdg.configFile."hypr/hyprland.lua".text = /* lua */ ''
          local mainMod   = "SUPER"
          local SECONDARY = "SHIFT"
          local TERTIARY  = "CTRL"

          local function k(...)
              return table.concat({ ... }, " + ")
          end

          local function toggle(program)
              local prog = string.sub(program, 1, 14)
              return "pkill " .. prog .. " || uwsm app -- " .. program
          end

          local function runOnce(program)
              return "pgrep " .. program .. " || uwsm app -- " .. program
          end

          local function launch(program)
              return "uwsm app -- " .. program
          end

          -- Floating + centered helper (50% w / 70% h, centered)
          local function centeredFloat(matchTbl)
              hl.window_rule({
                  match = matchTbl,
                  float = true,
                  size  = "monitor_w*0.5 monitor_h*0.7",
                  move  = "monitor_w*0.25 monitor_h*0.15",
              })
          end

          ----------------------------------------
          -- Helpers: parse hyprlang-style strings
          -- coming from the per-host Nix config.
          ----------------------------------------

          local function splitCommas(s)
              local result, from = {}, 1
              while true do
                  local i = string.find(s, ",", from, true)
                  if not i then
                      table.insert(result, (string.gsub(string.sub(s, from), "^%s*(.-)%s*$", "%1")))
                      break
                  end
                  table.insert(result, (string.gsub(string.sub(s, from, i - 1), "^%s*(.-)%s*$", "%1")))
                  from = i + 1
              end
              return result
          end

          local function applyMonitor(spec)
              local p = splitCommas(spec)
              local output = p[1] or ""
              local mode = p[2] or "preferred"
              if mode == "disable" or mode == "disabled" then
                  hl.monitor({ output = output, disabled = true })
                  return
              end
              local position = p[3] or "auto"
              local scale_str = p[4] or "1"
              local scale = tonumber(scale_str) or scale_str
              hl.monitor({
                  output   = output,
                  mode     = mode,
                  position = position,
                  scale    = scale,
              })
          end

          -- "1, monitor:DP-1" or "1, monitor:desc:Foo Bar"
          local _seenMonitorDefault = {}
          local function applyWorkspaceMonitor(spec)
              local id, rest = string.match(spec, "^%s*([^,]+)%s*,%s*(.*)$")
              if not id then return end
              local mon = string.match(rest, "^monitor:(.+)$")
              if not mon then return end
              mon = (string.gsub(mon, "^%s*(.-)%s*$", "%1"))
              local isDefault = not _seenMonitorDefault[mon]
              _seenMonitorDefault[mon] = true
              hl.workspace_rule({
                  workspace = id,
                  monitor   = mon,
                  default   = isDefault,
              })
          end

          ------------------
          ----- MONITORS -----
          ------------------

          for _, spec in ipairs(${toLuaList monitorList}) do
              applyMonitor(spec)
          end
          for _, spec in ipairs(${toLuaList workspaceList}) do
              applyWorkspaceMonitor(spec)
          end

          ---------------------------------
          ----- ENVIRONMENT VARIABLES -----
          ---------------------------------

          hl.env("GRIMBLAST_NO_CURSOR", "0")
          hl.env("HYPRCURSOR_THEME", "${pkgs.capitaine-cursors}")
          hl.env("HYPRCURSOR_SIZE", "16")
          hl.env("QT_WAYLAND_DISABLE_WINDOWDECORATION", "1")

          -------------------
          ----- AUTOSTART -----
          -------------------

          hl.on("hyprland.start", function()
              hl.exec_cmd("hyprpaper")
              hl.exec_cmd("hyprctl setcursor capitaine-cursors-white 16")
              hl.exec_cmd("wl-clip-persist --clipboard both &")
              hl.exec_cmd("wl-paste --watch cliphist store &")
              hl.exec_cmd("uwsm finalize")

              hl.exec_cmd("uwsm app -- zen",                  { workspace = "1" })
              hl.exec_cmd("uwsm app -- emacsclient -c -n",    { workspace = "2" })
              hl.exec_cmd("uwsm app -- slack",                { workspace = "8" })
              hl.exec_cmd("uwsm app -- discord",              { workspace = "9" })
          end)

          -------------------------
          ----- LOOK AND FEEL -----
          -------------------------

          hl.config({
              general = {
                  layout      = "master",
                  gaps_in     = 7,
                  gaps_out    = 7,
                  border_size = 2,
                  col = {
                      active_border   = { colors = { "rgb(${palette.base0E})", "rgb(${palette.base0F})", "rgb(${palette.base0D})" }, angle = 45 },
                      inactive_border = "rgb(${palette.base02})",
                  },
                  hover_icon_on_border    = true,
                  extend_border_grab_area = 15,
                  allow_tearing           = true,
                  resize_on_border        = true,
              },

              cursor = {
                  inactive_timeout    = 5,
                  no_hardware_cursors = true,
              },

              decoration = {
                  rounding = 16,
                  blur = {
                      enabled            = true,
                      size               = 4,
                      passes             = 2,
                      new_optimizations  = true,
                      ignore_opacity     = true,
                      xray               = false,
                      contrast           = 1.1,
                      brightness         = 1.0,
                      noise              = 0.02,
                  },
                  active_opacity     = 1.0,
                  inactive_opacity   = 0.95,
                  fullscreen_opacity = 1.0,
              },

              animations = {
                  enabled = true,
              },

              group = {
                  groupbar = {
                      font_size = 10,
                      gradients = false,
                  },
              },

              input = {
                  kb_layout  = "us,no",
                  kb_options = "grp:alt_shift_toggle",
              },

              master = {
                  new_status        = "slave",
                  new_on_top        = false,
                  mfact             = 0.55,
                  orientation       = "left",
                  allow_small_split = false,
                  smart_resizing    = true,
                  drop_at_cursor    = true,
              },

              misc = {
                  disable_autoreload           = true,
                  force_default_wallpaper      = 0,
                  animate_mouse_windowdragging = false,
                  vrr                          = 1,
                  on_focus_under_fullscreen    = true,
              },

              xwayland = {
                  force_zero_scaling = true,
              },

              debug = {
                  disable_logs = false,
              },
          })

          ----------------
          ---- CURVES ----
          ----------------

          hl.curve("wind",     { type = "bezier", points = { { 0.05, 0.9 },  { 0.1,  1.05 } } })
          hl.curve("winIn",    { type = "bezier", points = { { 0.1,  1.1 },  { 0.1,  1.1 } } })
          hl.curve("winOut",   { type = "bezier", points = { { 0.3, -0.3 },  { 0,    1 } } })
          hl.curve("liner",    { type = "bezier", points = { { 1,    1 },    { 1,    1 } } })
          hl.curve("overshot", { type = "bezier", points = { { 0.13, 0.99 }, { 0.29, 1.1 } } })

          --------------------
          ---- ANIMATIONS ----
          --------------------

          hl.animation({ leaf = "windows",          enabled = true, speed = 6,  bezier = "wind",     style = "slide" })
          hl.animation({ leaf = "windowsIn",        enabled = true, speed = 6,  bezier = "winIn",    style = "slide" })
          hl.animation({ leaf = "windowsOut",       enabled = true, speed = 5,  bezier = "winOut",   style = "slide" })
          hl.animation({ leaf = "windowsMove",      enabled = true, speed = 5,  bezier = "wind",     style = "slide" })
          hl.animation({ leaf = "border",           enabled = true, speed = 10, bezier = "liner" })
          hl.animation({ leaf = "borderangle",      enabled = true, speed = 60, bezier = "liner",    style = "loop" })
          hl.animation({ leaf = "fade",             enabled = true, speed = 10, bezier = "default" })
          hl.animation({ leaf = "workspaces",       enabled = true, speed = 6,  bezier = "overshot", style = "slidevert" })
          hl.animation({ leaf = "specialWorkspace", enabled = true, speed = 6,  bezier = "default",  style = "slidevert" })

          ---------------------
          ---- LAYER RULES ----
          ---------------------

          for _, ns in ipairs({
              "quickshell-bar",
              "quickshell-dashboard",
              "quickshell-notifications",
              "quickshell-launcher",
              "quickshell-session",
              "quickshell-lock",
          }) do
              hl.layer_rule({
                  match        = { namespace = "^(" .. ns .. ")$" },
                  blur         = true,
                  ignore_alpha = 0,
              })
          end

          ----------------------
          ----- KEYBINDINGS -----
          ----------------------

          -- Apps / launchers
          hl.bind(k(mainMod, "Return"),               hl.dsp.exec_cmd(launch("alacritty")))
          hl.bind(k(mainMod, "D"),                    hl.dsp.exec_cmd("qs -c bivrost ipc call launcher toggle"))
          hl.bind(k(mainMod, "B"),                    hl.dsp.exec_cmd(toggle("alacritty -t btop -e btm")))
          hl.bind(k(mainMod, "R"),                    hl.dsp.exec_cmd(toggle("alacritty -t ranger -e ranger")))
          hl.bind(k(mainMod, "S"),                    hl.dsp.exec_cmd(toggle("alacritty -t spotify_player -e spotify_player")))
          hl.bind(k(mainMod, SECONDARY, "D"),         hl.dsp.exec_cmd(runOnce("pcmanfm")))
          hl.bind(k(mainMod, SECONDARY, "L"),         hl.dsp.exec_cmd("qs -c bivrost ipc call lock lock"))
          hl.bind(k(mainMod, "A"),                    hl.dsp.exec_cmd("qs -c bivrost ipc call dashboard toggle"))
          hl.bind(k(mainMod, "Escape"),               hl.dsp.exec_cmd("qs -c bivrost ipc call session toggle"))
          hl.bind(k(mainMod, SECONDARY, "P"),         hl.dsp.exec_cmd(runOnce("grimblast --notify copy area")))

          -- Special workspace
          hl.bind(k(mainMod, SECONDARY, "T"),         hl.dsp.window.move({ workspace = "special" }))
          hl.bind(k(mainMod, "t"),                    hl.dsp.workspace.toggle_special(""))

          -- Session
          -- NOTE: under uwsm, prefer `uwsm stop` over hl.dsp.exit() for clean shutdown
          hl.bind(k(mainMod, SECONDARY, TERTIARY, "Q"), hl.dsp.exec_cmd("uwsm stop"))

          -- Window
          hl.bind(k(mainMod, "Q"),                    hl.dsp.window.close())
          hl.bind(k(mainMod, "F"),                    hl.dsp.window.float({ action = "toggle" }))
          hl.bind(k(mainMod, "G"),                    hl.dsp.window.fullscreen({ action = "toggle" }))

          -- Focus
          hl.bind(k(mainMod, "k"),                    hl.dsp.focus({ direction = "u" }))
          hl.bind(k(mainMod, "j"),                    hl.dsp.focus({ direction = "d" }))
          hl.bind(k(mainMod, "l"),                    hl.dsp.focus({ direction = "r" }))
          hl.bind(k(mainMod, "h"),                    hl.dsp.focus({ direction = "l" }))

          -- Swap windows
          hl.bind(k(mainMod, "ALT", "k"),             hl.dsp.window.swap({ direction = "u" }))
          hl.bind(k(mainMod, "ALT", "j"),             hl.dsp.window.swap({ direction = "d" }))
          hl.bind(k(mainMod, "ALT", "l"),             hl.dsp.window.swap({ direction = "r" }))
          hl.bind(k(mainMod, "ALT", "h"),             hl.dsp.window.swap({ direction = "l" }))

          -- Workspace navigation
          hl.bind(k(mainMod, "left"),                 hl.dsp.focus({ workspace = "e-1" }))
          hl.bind(k(mainMod, "right"),                hl.dsp.focus({ workspace = "e+1" }))
          hl.bind(k(mainMod, SECONDARY, "left"),      hl.dsp.window.move({ workspace = "e-1" }))
          hl.bind(k(mainMod, SECONDARY, "right"),     hl.dsp.window.move({ workspace = "e+1" }))

          -- Numbered workspaces 1..9
          for i = 1, 9 do
              hl.bind(k(mainMod, tostring(i)),            hl.dsp.focus({ workspace = tostring(i) }))
              hl.bind(k(mainMod, SECONDARY, tostring(i)), hl.dsp.window.move({ workspace = tostring(i) }))
          end

          -- Resize (repeatable)
          hl.bind(k(mainMod, TERTIARY, "k"), hl.dsp.window.resize({ x = 0,   y = -20, relative = true }), { repeating = true })
          hl.bind(k(mainMod, TERTIARY, "j"), hl.dsp.window.resize({ x = 0,   y = 20,  relative = true }), { repeating = true })
          hl.bind(k(mainMod, TERTIARY, "l"), hl.dsp.window.resize({ x = 20,  y = 0,   relative = true }), { repeating = true })
          hl.bind(k(mainMod, TERTIARY, "h"), hl.dsp.window.resize({ x = -20, y = 0,   relative = true }), { repeating = true })

          -- Mouse binds
          hl.bind(k(mainMod, "mouse:272"), hl.dsp.window.drag(),   { mouse = true })
          hl.bind(k(mainMod, "mouse:273"), hl.dsp.window.resize(), { mouse = true })

          ----------------------
          ---- WINDOW RULES ----
          ----------------------

          -- Floating + centered: by class
          for _, cls in ipairs({
              "Rofi",
              "eww",
              "Gimp-2.10",
              "pavucontrol",
              "nm-connection-editor",
              "Color Picker",
              "Network",
              "pcmanfm",
              "com.github.flxzt.rnote",
              "xdg-desktop-portal",
              "xdg-desktop-portal-gnome",
              "transmission-gtk",
              "org.kde.kdeconnect-settings",
              "org.pulseaudio.pavucontrol",
          }) do
              centeredFloat({ class = "^(" .. cls .. ")$" })
          end

          -- Floating + centered: by title
          for _, t in ipairs({
              "Spotify Premium",
              "Proton Pass",
              "Spotify",
              "spotify_player",
              "ranger",
              "btop",
          }) do
              centeredFloat({ title = "^(" .. t .. ")$" })
          end

          -- Per-app opacity
          hl.window_rule({
              match   = { class = "^(Emacs)$" },
              opacity = "0.91 override 0.73 override",
          })

          for _, r in ipairs({
              { match = { class = "^(zen)$" },                   workspace = "1" },
              { match = { class = "^(Emacs)$" },                 workspace = "2" },
              { match = { class = "^(Alacritty)$" },             workspace = "3" },
              { match = { class = "^(Wfica)$" },                 workspace = "5" },
              { match = { class = "^(.virt-manager-wrapped)$" }, workspace = "5" },
              { match = { class = "^(steam)$" },                 workspace = "7" },
              { match = { title = "^(Friends List)$" },          workspace = "7" },
              { match = { class = "^(Slack)$" },                 workspace = "8" },
              { match = { class = "^(discord)$" },               workspace = "9" },
          }) do
              hl.window_rule(r)
          end
        '';

        home = {
          packages = with pkgs; [
            arandr
            bottom
            ffmpegthumbnailer
            glib
            headsetcontrol
            imagemagick
            libnotify
            paprefs
            pavucontrol
            poppler
            pulsemixer
            ranger
            scrot
            slurp
            wayshot
            wgetpaste
            wl-clipboard
            wl-gammactl
          ];
          sessionVariables = {
            SSH_AUTH_SOCK = "/run/user/1000/keyring/ssh";
            XDG_CURRENT_DESKTOP = "Hyprland";
            XDG_SESSION_DESKTOP = "Hyprland";
            XDG_SESSION_TYPE = "wayland";
            SDL_VIDEODRIVER = "wayland";
            QT_AUTO_SCREEN_SCALE_FACTOR = 1;
            QT_QPA_PLATFORM = "wayland;xcb";
            QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
            QT_QPA_PLATFORMTHEME = "qt5ct";
          };
        };
      };
    };
}
