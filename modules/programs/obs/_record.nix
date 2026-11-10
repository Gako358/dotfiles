{
  pkgs,
  obs,
}:
pkgs.writeShellApplication {
  name = "obs-record";
  runtimeInputs = with pkgs; [
    coreutils
    jq
    libnotify
    obs-cmd
    procps
  ];
  text = ''
    cfg="''${XDG_CONFIG_HOME:-$HOME/.config}/obs-studio/plugin_config/obs-websocket/config.json"

    obs_running() {
      pgrep -u "$(id -u)" -x '(\.obs-wrapped|obs)' >/dev/null
    }

    notify() {
      notify-send -a "OBS" -i com.obsproject.Studio "$@" || true
    }

    # The firewall is off on these hosts and obs-websocket listens on all
    # interfaces, so the server must always require a password.
    ensure_websocket() {
      mkdir -p "$(dirname "$cfg")"
      [ -s "$cfg" ] || echo '{}' >"$cfg"
      local password tmp
      password=$(jq -r '.server_password // ""' "$cfg")
      if ! [[ "$password" =~ ^[A-Za-z0-9]{16,}$ ]]; then
        password=$(od -An -tx1 -N16 /dev/urandom | tr -d ' \n')
      fi
      tmp=$(mktemp "$cfg.XXXXXX")
      jq --arg pw "$password" '. + {
        first_load: false,
        server_enabled: true,
        auth_required: true,
        server_password: $pw,
        server_port: (.server_port // 4455),
        alerts_enabled: (.alerts_enabled // false)
      }' "$cfg" >"$tmp"
      chmod 600 "$tmp"
      mv "$tmp" "$cfg"
    }

    websocket_url() {
      [ -s "$cfg" ] || return 1
      jq -r '"obsws://127.0.0.1:\(.server_port // 4455)/\(.server_password // "")"' "$cfg"
    }

    obsctl() {
      local url
      url=$(websocket_url) || return 1
      OBS_WEBSOCKET_URL="$url" obs-cmd "$@"
    }

    status() {
      if ! obs_running; then
        echo idle
        return
      fi
      local out
      out=$(obsctl recording status-active 2>&1 || true)
      case "$out" in
        *"active and running"*) echo recording ;;
        *"currently paused"*) echo paused ;;
        *) echo idle ;;
      esac
    }

    toggle() {
      if ! obs_running; then
        ensure_websocket
        uwsm app -t service -- ${obs}/bin/obs \
          --minimize-to-tray --startrecording --disable-shutdown-check
        notify "Recording started" "OBS is starting in the tray"
        return
      fi
      local before
      before=$(status)
      if ! obsctl recording toggle >/dev/null 2>&1; then
        ensure_websocket
        notify -u critical "Recording failed" \
          "Could not reach the OBS websocket. Restart OBS to enable remote control."
        exit 1
      fi
      case "$before" in
        idle) notify "Recording started" ;;
        *) notify "Recording stopped" "Saved to the OBS recording path" ;;
      esac
    }

    case "''${1:-toggle}" in
      toggle) toggle ;;
      status) status ;;
      *)
        echo "Usage: obs-record [toggle|status]" >&2
        exit 2
        ;;
    esac
  '';
}
