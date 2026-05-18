{
  c,
  ca,
  lib,
  battery ? false,
}:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Widgets
  import Quickshell.Hyprland
  import Quickshell.Io
  import Quickshell.Services.Pipewire
  import Quickshell.Services.SystemTray
  import Quickshell.Wayland
  ${lib.optionalString battery "import Quickshell.Services.UPower"}

  PanelWindow {
      id: bar

      signal launcherRequested()
      signal dashboardRequested()
      signal calendarRequested()
      signal sessionRequested()
      signal systemMonitorRequested()
      signal audioRequested()
      signal networkRequested()
      signal trayRequested()
      signal wallpaperRequested()

      // ── Data properties ───────────────────────────────────────────
      property string netState:  "off"
      property int    netSignal: 0
      property string netSsid:   ""

      Process {
          id: barNetProc
          command: ["sh", "-c",
              "act=$(nmcli -t -f TYPE,STATE,CONNECTION device status 2>/dev/null | awk -F: '$2==\"connected\"{print $1\":\"$3; exit}'); " +
              "if echo \"$act\" | grep -q '^wifi:'; then " +
              "  ssid=$(echo \"$act\" | cut -d: -f2-); " +
              "  sig=$(nmcli -t -f IN-USE,SIGNAL device wifi 2>/dev/null | awk -F: '$1==\"*\"{print $2; exit}'); " +
              "  echo \"wifi|$sig|$ssid\"; " +
              "elif echo \"$act\" | grep -q '^ethernet:'; then " +
              "  echo \"wired|100|$(echo \"$act\" | cut -d: -f2-)\"; " +
              "else echo \"off|0|\"; fi"]
          stdout: StdioCollector { id: barNetOut }
          onExited: {
              var parts = ((barNetOut.text || "").trim() || "off|0|").split("|")
              bar.netState  = parts[0] || "off"
              bar.netSignal = parseInt(parts[1] || "0") || 0
              bar.netSsid   = parts[2] || ""
          }
      }

      Timer {
          running: true
          repeat: true
          interval: 5000
          triggeredOnStart: true
          onTriggered: barNetProc.running = true
      }

      PwObjectTracker {
          objects: Pipewire.defaultAudioSink ? [Pipewire.defaultAudioSink] : []
      }

      readonly property var audioSink: Pipewire.defaultAudioSink
      readonly property real volPct:   audioSink && audioSink.audio ? audioSink.audio.volume * 100 : 0
      readonly property bool muted:    audioSink && audioSink.audio ? audioSink.audio.muted : true

      ${lib.optionalString battery ''
        readonly property var batDevice:  UPower.displayDevice
        readonly property bool hasBat:
            batDevice && batDevice.isPresent
            && batDevice.type === UPowerDeviceType.Battery
        readonly property real batPct:      hasBat ? batDevice.percentage * 100 : 0
        readonly property bool batCharging:
            hasBat && (batDevice.state === UPowerDeviceState.Charging
                    || batDevice.state === UPowerDeviceState.FullyCharged)
      ''}

      // ── Helpers ───────────────────────────────────────────────────
      function volIcon(p, m) {
          if (m || p <= 0) return "󰝟"
          if (p < 34)      return "󰕿"
          if (p < 67)      return "󰖀"
          return "󰕾"
      }
      function wifiIcon(s) {
          if (s >= 75) return "󰤨"
          if (s >= 50) return "󰤥"
          if (s >= 25) return "󰤢"
          if (s > 0)   return "󰤟"
          return "󰤮"
      }
      ${lib.optionalString battery ''
        function batteryIcon(p, charging) {
            if (charging) return "󰂄"
            if (p >= 95) return "󰁹"
            if (p >= 80) return "󰂂"
            if (p >= 65) return "󰂀"
            if (p >= 50) return "󰁾"
            if (p >= 35) return "󰁼"
            if (p >= 20) return "󰁻"
            if (p >= 10) return "󰁺"
            return "󰂎"
        }
        function batteryColor(p, charging) {
            if (charging) return "${c "base0B"}"
            if (p <= 15)  return "${c "base08"}"
            if (p <= 30)  return "${c "base0A"}"
            return "${c "base0B"}"
        }
      ''}

      // ── Shell geometry ────────────────────────────────────────────
      WlrLayershell.namespace: "quickshell-bar"

      readonly property bool isFocused:
          Hyprland.focusedMonitor !== null
          && bar.screen !== null
          && Hyprland.focusedMonitor.name === bar.screen.name

      anchors {
          bottom: true
          left: true
          right: true
      }
      margins {
          bottom: 0
          left: 0
          right: 0
      }

      implicitHeight: 48
      exclusiveZone: 48
      color: "transparent"

      // ── Background ────────────────────────────────────────────────
      Rectangle {
          anchors.fill: parent
          // Win11-style: slightly translucent strip, no rounding on bar itself
          color: "${ca "base00" "b8"}"
          border.width: 0

          // ── Top border line (Win11 thin separator) ───────────────
          Rectangle {
              anchors.top: parent.top
              anchors.left: parent.left
              anchors.right: parent.right
              height: 1
              color: "${ca "base02" "88"}"
          }

          // ══════════════════════════════════════════════════════════
          // CENTER ZONE — NixOS logo + dock
          // ══════════════════════════════════════════════════════════
          Row {
              id: centerZone
              anchors.horizontalCenter: parent.horizontalCenter
              anchors.verticalCenter: parent.verticalCenter
              spacing: 2

              // ── NixOS / launcher button ──────────────────────────
              Item {
                  id: nixBtn
                  width: 42
                  height: 42

                  Rectangle {
                      anchors.fill: parent
                      radius: 8
                      color: nixHover.hovered
                          ? "${ca "base0D" "30"}"
                          : "transparent"
                      Behavior on color { ColorAnimation { duration: 150 } }
                      HoverHandler { id: nixHover }

                      Text {
                          anchors.centerIn: parent
                          text: "󱄅"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 26
                          color: nixHover.hovered
                              ? "${c "base0D"}"
                              : "${ca "base0D" "cc"}"
                          Behavior on color { ColorAnimation { duration: 150 } }
                      }
                  }
                  MouseArea {
                      anchors.fill: parent
                      cursorShape: Qt.PointingHandCursor
                      onClicked: bar.launcherRequested()
                  }
              }

              // ── Thin separator ───────────────────────────────────
              Rectangle {
                  width: 1
                  height: 28
                  anchors.verticalCenter: parent.verticalCenter
                  color: "${ca "base02" "88"}"
              }

              // ── Pinned app dock ──────────────────────────────────
              // Apps: emacs, alacritty, zen, pcmanfm, proton pass, discord, slack
              Row {
                  id: dockRow
                  anchors.verticalCenter: parent.verticalCenter
                  spacing: 2

                  Repeater {
                      model: [
                          { icon: "󰅴", label: "Emacs",       cmd: "emacs" },
                          { icon: "󰆍", label: "Alacritty",   cmd: "alacritty" },
                          { icon: "󰈹", label: "Zen Browser", cmd: "zen" },
                          { icon: "󰝰", label: "Files",       cmd: "pcmanfm" },
                          { icon: "󰦝", label: "Proton Pass",  cmd: "proton-pass" },
                          { icon: "󰙯", label: "Discord",     cmd: "discord" },
                          { icon: "󰒱", label: "Slack",       cmd: "slack" }
                      ]

                      Item {
                          id: dockItem
                          required property var modelData
                          required property int index
                          width: 44
                          height: 44

                          property bool hov: dockItemHover.hovered

                          Rectangle {
                              id: dockItemBg
                              anchors.centerIn: parent
                              width: dockItem.hov ? 38 : 34
                              height: dockItem.hov ? 38 : 34
                              radius: 8
                              color: dockItem.hov
                                  ? "${ca "base02" "cc"}"
                                  : "transparent"
                              Behavior on width  { NumberAnimation { duration: 150; easing.type: Easing.OutQuint } }
                              Behavior on height { NumberAnimation { duration: 150; easing.type: Easing.OutQuint } }
                              Behavior on color  { ColorAnimation  { duration: 120 } }

                              // Vertical offset (lift on hover)
                              property real offsetY: dockItem.hov ? -3 : 0
                              transform: Translate { y: dockItemBg.offsetY }
                              Behavior on offsetY {
                                  NumberAnimation { duration: 150; easing.type: Easing.OutQuint }
                              }

                              HoverHandler { id: dockItemHover }

                              Text {
                                  anchors.centerIn: parent
                                  text: dockItem.modelData.icon
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 20
                                  color: dockItem.hov ? "${c "base05"}" : "${c "base06"}"
                                  Behavior on color { ColorAnimation { duration: 120 } }
                              }

                              // Tooltip
                              ToolTip {
                                  visible: dockItem.hov
                                  delay: 500
                                  text: dockItem.modelData.label
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                              }
                          }

                          // Running dot indicator
                          Rectangle {
                              anchors.horizontalCenter: parent.horizontalCenter
                              anchors.bottom: parent.bottom
                              anchors.bottomMargin: 1
                              width: 4
                              height: 4
                              radius: 2
                              color: "${c "base0D"}"
                              visible: false // TODO: connect to window tracking
                          }

                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: {
                                  var proc = Qt.createQmlObject(
                                      "import Quickshell.Io; Process { command: [\"sh\", \"-c\", \"" +
                                      dockItem.modelData.cmd + " &\"]; running: true }",
                                      dockItem, "dockLaunch")
                              }
                          }
                      }
                  }
              }
          }

          // ══════════════════════════════════════════════════════════
          // RIGHT ZONE — system tray corner + clock (opens dashboard)
          // ══════════════════════════════════════════════════════════
          Row {
              id: rightZone
              anchors.right: parent.right
              anchors.verticalCenter: parent.verticalCenter
              anchors.rightMargin: 6
              spacing: 2
              visible: bar.isFocused

              // ── Win11-style system tray corner ───────────────────
              // Shows up to 4 tray icons in a 2×2 grid, click to open
              // the full tray panel. Styled like Win11's corner icons.
              Item {
                  id: trayCorner
                  property var items: SystemTray.items ? SystemTray.items.values : []
                  property int trayCount: items.length
                  // Width: fits 2 icon columns (14px each) + padding
                  width: trayCorner.trayCount > 0 ? 44 : 32
                  height: 42
                  Behavior on width { NumberAnimation { duration: 150 } }

                  Rectangle {
                      anchors.fill: parent
                      radius: 6
                      color: trayCornerHover.hovered
                          ? "${ca "base02" "cc"}"
                          : "transparent"
                      Behavior on color { ColorAnimation { duration: 120 } }
                      HoverHandler { id: trayCornerHover }

                      // When there are actual tray items: show up to 4 icons in 2×2 grid
                      Grid {
                          visible: trayCorner.trayCount > 0
                          anchors.centerIn: parent
                          columns: 2
                          rows: 2
                          spacing: 2

                          Repeater {
                              model: Math.min(trayCorner.items.length, 4)
                              Item {
                                  width: 14
                                  height: 14
                                  required property int index
                                  property var trayItem: trayCorner.items[index] || null

                                  IconImage {
                                      anchors.fill: parent
                                      source: parent.trayItem
                                          ? (parent.trayItem.icon || "")
                                          : ""
                                      implicitSize: 14
                                  }
                              }
                          }
                      }

                      // When empty: show a subtle expand chevron
                      Text {
                          visible: trayCorner.trayCount === 0
                          anchors.centerIn: parent
                          text: "󰅃"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 13
                          color: "${c "base04"}"
                      }

                      // Overflow indicator dot when >4 items
                      Rectangle {
                          visible: trayCorner.trayCount > 4
                          width: overflowText.implicitWidth + 6
                          height: 12
                          radius: 6
                          color: "${c "base0D"}"
                          anchors.right: parent.right
                          anchors.bottom: parent.bottom
                          anchors.rightMargin: 1
                          anchors.bottomMargin: 1
                          Text {
                              id: overflowText
                              anchors.centerIn: parent
                              text: "+" + (trayCorner.trayCount - 4)
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 8
                              color: "${c "base00"}"
                          }
                      }
                  }

                  MouseArea {
                      anchors.fill: parent
                      cursorShape: Qt.PointingHandCursor
                      onClicked: bar.trayRequested()
                  }
              }

              // ── Thin separator ───────────────────────────────────
              Rectangle {
                  width: 1
                  height: 26
                  anchors.verticalCenter: parent.verticalCenter
                  color: "${ca "base02" "66"}"
              }

              // ── Clock block (opens dashboard / action center) ────
              Item {
                  id: clockItem
                  width: clockCol.implicitWidth + 20
                  height: 42

                  Rectangle {
                      anchors.fill: parent
                      radius: 6
                      color: clockHover.hovered
                          ? "${ca "base02" "cc"}"
                          : "transparent"
                      Behavior on color { ColorAnimation { duration: 120 } }
                      HoverHandler { id: clockHover }

                      Column {
                          id: clockCol
                          anchors.centerIn: parent
                          spacing: 0

                          Text {
                              id: clockTime
                              anchors.horizontalCenter: parent.horizontalCenter
                              text: ""
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 14
                              font.weight: Font.Medium
                              color: "${c "base05"}"
                              Timer {
                                  running: true
                                  repeat: true
                                  interval: 1000
                                  triggeredOnStart: true
                                  onTriggered: clockTime.text =
                                      Qt.formatDateTime(new Date(), "hh:mm AP")
                              }
                          }
                          Text {
                              id: clockDate
                              anchors.horizontalCenter: parent.horizontalCenter
                              text: ""
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 10
                              color: "${c "base04"}"
                              Timer {
                                  running: true
                                  repeat: true
                                  interval: 60000
                                  triggeredOnStart: true
                                  onTriggered: clockDate.text =
                                      Qt.formatDateTime(new Date(), "dd/MM/yyyy")
                              }
                          }
                      }
                  }

                  MouseArea {
                      anchors.fill: parent
                      cursorShape: Qt.PointingHandCursor
                      onClicked: bar.dashboardRequested()
                  }
              }

              // ── Show-desktop strip (Win11 rightmost edge) ────────
              Rectangle {
                  width: 4
                  height: 42
                  radius: 2
                  color: showDeskHover.hovered
                      ? "${ca "base0D" "88"}"
                      : "transparent"
                  Behavior on color { ColorAnimation { duration: 120 } }
                  HoverHandler { id: showDeskHover }
                  ToolTip {
                      visible: showDeskHover.hovered
                      delay: 600
                      text: "Show desktop"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 11
                  }
                  MouseArea {
                      anchors.fill: parent
                      cursorShape: Qt.PointingHandCursor
                      onClicked: Hyprland.dispatch("togglespecialworkspace")
                  }
              }
          }
      }
  }
''
