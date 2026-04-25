{ inputs, ... }:
{
  # NixOS side: enable Hyprland (compositor), set keybindings, window rules, and full settings.
  flake.nixosModules.programs-hyprland =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    let
      mainMod = "SUPER";
      SECONDARY = "SHIFT";
      TERTIARY = "CTRL";

      toggle =
        program:
        let
          prog = builtins.substring 0 14 program;
        in
        "pkill ${prog} || uwsm app -- ${program}";

      runOnce = program: "pgrep ${program} || uwsm app -- ${program}";
      launch = program: "uwsm app -- ${program}";
    in
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

          settings = {
            # Environment / autostart / look-and-feel (was system/services/hyprland/settings.nix)
            env = [
              "GRIMBLAST_NO_CURSOR,0"
              "HYPRCURSOR_THEME,${pkgs.capitaine-cursors}"
              "HYPRCURSOR_SIZE,16"
              "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
            ];
            exec-once = [
              "hyprpaper"
              "hyprctl setcursor capitaine-cursors-white 16"
              "wl-clip-persist --clipboard both &"
              "wl-paste --watch cliphist store &"
              "uwsm finalize"
              "[workspace 1 silent] zen"
              "[workspace 2 silent] emacsclient -c -n"
              "[workspace 8 silent] slack"
              "[workspace 9 silent] discord"
            ];
            general = {
              gaps_in = 7;
              gaps_out = 7;
              border_size = 2;
              "col.active_border" = "rgb(B48EAD) rgb(5E81AC) rgb(719cd6) 45deg";
              "col.inactive_border" = "rgb(3B4252)";
              hover_icon_on_border = true;
              extend_border_grab_area = 15;
              allow_tearing = true;
              resize_on_border = true;
            };
            cursor.inactive_timeout = 5;
            decoration = {
              rounding = 16;
              blur = {
                enabled = true;
                size = 4;
                passes = 2;
                new_optimizations = true;
                ignore_opacity = true;
                xray = false;
                contrast = 1.1;
                brightness = 1.0;
                noise = 0.02;
              };
              active_opacity = 1.0;
              inactive_opacity = 0.95;
              fullscreen_opacity = 1.0;
            };
            layerrule = [
              "blur on, match:namespace ^(wofi)$"
              "ignore_alpha 0, match:namespace ^(wofi)$"
              "blur on, match:namespace ^(waybar)$"
              "ignore_alpha 0, match:namespace ^(waybar)$"
              "blur on, match:namespace ^(notifications)$"
              "ignore_alpha 0, match:namespace ^(notifications)$"
            ];
            animations.enabled = true;
            animation = [
              "windows, 1, 6, wind, slide"
              "windowsIn, 1, 6, winIn, slide"
              "windowsOut, 1, 5, winOut, slide"
              "windowsMove, 1, 5, wind, slide"
              "border, 1, 10, liner"
              "borderangle, 1, 60, liner, loop"
              "fade, 1, 10, default"
              "workspaces, 1, 6, overshot, slidevert"
              "specialWorkspace, 1, 6, default, slidevert"
            ];
            bezier = [
              "wind, 0.05, 0.9, 0.1, 1.05"
              "winIn, 0.1, 1.1, 0.1, 1.1"
              "winOut, 0.3, -0.3, 0, 1"
              "liner, 1, 1, 1, 1"
              "overshot, 0.13, 0.99, 0.29, 1.1"
            ];
            group = {
              groupbar = {
                font_size = 10;
                gradients = false;
              };
            };
            input = {
              kb_layout = "us,no";
              kb_options = "grp:alt_shift_toggle";
            };
            dwindle = {
              pseudotile = true;
              preserve_split = true;
            };
            misc = {
              disable_autoreload = true;
              force_default_wallpaper = 0;
              animate_mouse_windowdragging = false;
              vrr = 1;
            };
            xwayland.force_zero_scaling = true;
            debug.disable_logs = false;

            # Keybindings (was system/services/hyprland/binds.nix)
            bind = [
              "${mainMod}, Return, exec, ${launch "alacritty"}"
              "${mainMod}, D, exec, ${toggle "wofi --show drun"}"
              "${mainMod}, B, exec, ${toggle "alacritty -t btop -e btm"}"
              "${mainMod}, R, exec, ${toggle "alacritty -t ranger -e ranger"}"
              "${mainMod}, S, exec, ${toggle "alacritty -t spotify_player -e spotify_player"}"
              "${mainMod} ${SECONDARY}, D, exec, ${runOnce "pcmanfm"}"
              "${mainMod} ${SECONDARY}, L, exec, ${runOnce "hyprlock"}"
              "${mainMod} ${SECONDARY}, P, exec, ${runOnce "grimblast --notify copy area"}"
              "${mainMod} ${SECONDARY}, T, movetoworkspace, special"
              "${mainMod}, t, togglespecialworkspace"
              "${mainMod} ${SECONDARY} ${TERTIARY}, Q, exit"
              "${mainMod}, Q, killactive"
              "${mainMod}, F, togglefloating"
              "${mainMod}, G, fullscreen"
              "${mainMod}, k, movefocus, u"
              "${mainMod}, j, movefocus, d"
              "${mainMod}, l, movefocus, r"
              "${mainMod}, h, movefocus, l"
              "${mainMod}, left,  workspace, e-1"
              "${mainMod}, right, workspace, e+1"
              "${mainMod}, 1, workspace, 1"
              "${mainMod}, 2, workspace, 2"
              "${mainMod}, 3, workspace, 3"
              "${mainMod}, 4, workspace, 4"
              "${mainMod}, 5, workspace, 5"
              "${mainMod}, 6, workspace, 6"
              "${mainMod}, 7, workspace, 7"
              "${mainMod}, 8, workspace, 8"
              "${mainMod}, 9, workspace, 9"
              "${mainMod} ${SECONDARY}, right, movetoworkspace, e+1"
              "${mainMod} ${SECONDARY}, left,  movetoworkspace, e-1"
              "${mainMod} ${SECONDARY}, 1, movetoworkspace, 1"
              "${mainMod} ${SECONDARY}, 2, movetoworkspace, 2"
              "${mainMod} ${SECONDARY}, 3, movetoworkspace, 3"
              "${mainMod} ${SECONDARY}, 4, movetoworkspace, 4"
              "${mainMod} ${SECONDARY}, 5, movetoworkspace, 5"
              "${mainMod} ${SECONDARY}, 6, movetoworkspace, 6"
              "${mainMod} ${SECONDARY}, 7, movetoworkspace, 7"
              "${mainMod} ${SECONDARY}, 8, movetoworkspace, 8"
              "${mainMod} ${SECONDARY}, 9, movetoworkspace, 9"
            ];
            binde = [
              "${mainMod} ${TERTIARY}, k, resizeactive, 0 -20"
              "${mainMod} ${TERTIARY}, j, resizeactive, 0 20"
              "${mainMod} ${TERTIARY}, l, resizeactive, 20 0"
              "${mainMod} ${TERTIARY}, h, resizeactive, -20 0"
              "${mainMod} ALT,  k, moveactive, 0 -20"
              "${mainMod} ALT,  j, moveactive, 0 20"
              "${mainMod} ALT,  l, moveactive, 20 0"
              "${mainMod} ALT,  h, moveactive, -20 0"
            ];
            bindm = [
              "${mainMod}, mouse:272, movewindow"
              "${mainMod}, mouse:273, resizewindow"
            ];

            # Window rules (was system/services/hyprland/rules.nix)
            windowrule = [
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(Rofi)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(eww)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(Gimp-2.10)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(org.gnome.Calculator)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(org.gnome.Calendar)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(gnome-system-monitor)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(pavucontrol)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(nm-connection-editor)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(Color Picker)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(Network)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(pcmanfm)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(com.github.flxzt.rnote)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(xdg-desktop-portal)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(xdg-desktop-portal-gnome)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(transmission-gtk)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(org.kde.kdeconnect-settings)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:class ^(org.pulseaudio.pavucontrol)$"

              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(Spotify Premium)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(Proton Pass)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(Spotify)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(spotify_player)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(ranger)$"
              "float on, size (monitor_w*0.5) (monitor_h*0.7), center on, match:title ^(btop)$"

              "opacity 0.91 override 0.73 override, match:class ^(Emacs)$"

              "workspace 1, match:class ^(zen)$"
              "workspace 2, match:class ^(Emacs)$"
              "workspace 3, match:class ^(Alacritty)$"
              "workspace 5, match:class ^(Wfica)$"
              "workspace 5, match:class ^(.virt-manager-wrapped)$"
              "workspace 7, match:class ^(steam)$"
              "workspace 7, match:title ^(Friends List)$"
              "workspace 8, match:class ^(Slack)$"
              "workspace 9, match:class ^(discord)$"
            ];
          };
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

  # home-manager side: per-user session vars + GUI helper packages on Hyprland hosts.
  flake.homeModules.programs-hyprland =
    {
      lib,
      pkgs,
      osConfig,
      ...
    }:
    {
      config = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
        home = {
          packages = with pkgs; [
            arandr
            bottom
            ffmpegthumbnailer
            glib
            gnome-calendar
            gnome-boxes
            gnome-weather
            gnome-system-monitor
            headsetcontrol
            imagemagick
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
