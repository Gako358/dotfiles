{
  c,
  ca,
  lib,
  battery ? false,
}:
''
  import QtQuick
  import QtQuick.Layouts
  import Quickshell
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

      property string cpuPct: "—"
      property string netState: "off"
      property int    netSignal: 0
      property string netSsid: ""

      Process {
          id: barCpuProc
          command: ["sh", "-c",
              "top -bn1 | awk '/Cpu/ { printf \"%.0f\", 100 - $8 }'"]
          stdout: StdioCollector { id: barCpuOut }
          onExited: bar.cpuPct = ((barCpuOut.text || "").trim() || "0")
      }

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
          interval: 3000
          triggeredOnStart: true
          onTriggered: {
              barCpuProc.running = true
              barNetProc.running = true
          }
      }

      PwObjectTracker {
          objects: Pipewire.defaultAudioSink ? [Pipewire.defaultAudioSink] : []
      }

      WlrLayershell.namespace: "quickshell-bar"

      readonly property bool isFocused:
          Hyprland.focusedMonitor !== null
          && bar.screen !== null
          && Hyprland.focusedMonitor.name === bar.screen.name

      anchors {
          top: true
          left: true
          right: true
      }
      margins {
          top: 4
          left: 8
          right: 8
      }

      implicitHeight: 36
      exclusiveZone: 40
      color: "transparent"

      Rectangle {
          anchors.fill: parent
          color: "${ca "base00" "75"}"
          radius: 12
          border.width: 1
          border.color: "${c "base02"}"

          Text {
              id: clockText
              visible: bar.isFocused
              text: ""
              font.family: "RobotoMono Nerd Font"
              font.pixelSize: 13
              color: "${c "base05"}"
              anchors.horizontalCenter: parent.horizontalCenter
              anchors.verticalCenter: parent.verticalCenter
              z: 1
              Timer {
                  running: true
                  repeat: true
                  interval: 1000
                  triggeredOnStart: true
                  onTriggered: clockText.text =
                      "󰥔 " + Qt.formatDateTime(new Date(), "ddd, dd MMM, hh:mm AP")
              }
              MouseArea {
                  anchors.fill: parent
                  cursorShape: Qt.PointingHandCursor
                  onClicked: bar.calendarRequested()
              }
          }

          RowLayout {
              anchors.fill: parent
              anchors.leftMargin: 14
              anchors.rightMargin: 14
              spacing: 14

              Item {
                  Layout.preferredWidth: 26
                  Layout.fillHeight: true
                  Text {
                      anchors.centerIn: parent
                      text: "󱄅"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 18
                      color: "${c "base0D"}"
                  }
                  MouseArea {
                      anchors.fill: parent
                      cursorShape: Qt.PointingHandCursor
                      onClicked: bar.launcherRequested()
                  }
              }

              // ── Workspaces ───────────────────────────────────
              Row {
                  Layout.alignment: Qt.AlignVCenter
                  spacing: 4
                  Repeater {
                      model: 9
                      Item {
                          id: ws
                          property int wsId: index + 1
                          property bool isActive: Hyprland.focusedWorkspace
                              && Hyprland.focusedWorkspace.id === wsId
                          property bool hasIcon: wsId === 1 || wsId === 2
                              || wsId === 3 || wsId === 5
                              || wsId === 8 || wsId === 9
                          property bool wsExists: {
                              var list = Hyprland.workspaces
                                  ? Hyprland.workspaces.values
                                  : []
                              for (var i = 0; i < list.length; ++i) {
                                  if (list[i] && list[i].id === ws.wsId)
                                      return true
                              }
                              return false
                          }
                          property string icon: {
                              switch (wsId) {
                                  case 1: return "󰈹"   // browsing  (nf-md-firefox)
                                  case 2: return "󰅴"   // coding    (nf-md-emacs)
                                  case 3: return "󰆍"   // terminal  (nf-md-console)
                                  case 5: return "󰢹"   // qemu / vm (nf-md-monitor)
                                  case 8: return "󰒱"   // slack     (nf-md-slack)
                                  case 9: return "󰙯"   // discord   (nf-md-discord)
                                  default: return wsId.toString()
                              }
                          }

                          visible: hasIcon || wsExists || isActive

                          width: isActive ? 34 : 22
                          height: 22

                          Behavior on width {
                              NumberAnimation { duration: 240; easing.type: Easing.OutQuint }
                          }

                          Rectangle {
                              anchors.fill: parent
                              radius: height / 2
                              color: ws.isActive
                                  ? "${c "base0D"}"
                                  : (wsHover.hovered
                                      ? "${ca "base02" "cc"}"
                                      : "transparent")
                              Behavior on color {
                                  ColorAnimation { duration: 180 }
                              }

                              HoverHandler { id: wsHover }

                              Text {
                                  anchors.centerIn: parent
                                  text: ws.icon
                                  color: ws.isActive
                                      ? "${c "base00"}"
                                      : "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: ws.isActive ? 14 : 11
                                  Behavior on font.pixelSize {
                                      NumberAnimation {
                                          duration: 240
                                          easing.type: Easing.OutQuint
                                      }
                                  }
                                  Behavior on color {
                                      ColorAnimation { duration: 180 }
                                  }
                              }
                          }

                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: Hyprland.dispatch("hl.dsp.focus({ workspace = " + ws.wsId + " })")
                          }
                      }
                  }
              }

              Item { Layout.fillWidth: true }

              // ── System tray (collapsed into a popup button) ──
              Item {
                  id: trayButton
                  visible: bar.isFocused
                  Layout.alignment: Qt.AlignVCenter
                  Layout.preferredHeight: 26
                  Layout.preferredWidth: 32
                  property int trayCount: SystemTray.items ? SystemTray.items.values.length : 0

                  Rectangle {
                      anchors.fill: parent
                      radius: 13
                      color: trayHover.hovered
                          ? "${ca "base02" "cc"}"
                          : "transparent"
                      Behavior on color { ColorAnimation { duration: 120 } }
                      HoverHandler { id: trayHover }

                      Text {
                          anchors.centerIn: parent
                          text: "󱊖"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 14
                          color: trayButton.trayCount > 0
                              ? "${c "base05"}"
                              : "${c "base04"}"
                      }

                      // Small badge with the number of tray items
                      Rectangle {
                          visible: trayButton.trayCount > 0
                          width: badgeText.implicitWidth + 6
                          height: 12
                          radius: 6
                          color: "${c "base0D"}"
                          anchors.right: parent.right
                          anchors.top: parent.top
                          anchors.rightMargin: 1
                          anchors.topMargin: 1
                          Text {
                              id: badgeText
                              anchors.centerIn: parent
                              text: trayButton.trayCount
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 9
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

              // ── Status pill (segmented, caelestia-style) ─────
              Rectangle {
                  id: statusPill
                  visible: bar.isFocused
                  Layout.alignment: Qt.AlignVCenter
                  Layout.preferredHeight: 26
                  Layout.preferredWidth: statusRow.implicitWidth + 6
                  color: "${ca "base01" "aa"}"
                  border.color: "${c "base02"}"
                  border.width: 1
                  radius: 13

                  readonly property var sink: Pipewire.defaultAudioSink
                  readonly property real volPct:
                      sink && sink.audio ? sink.audio.volume * 100 : 0
                  readonly property bool muted:
                      sink && sink.audio ? sink.audio.muted : true

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
                    readonly property var battery: UPower.displayDevice
                    readonly property bool hasBattery:
                        statusPill.battery
                        && statusPill.battery.isPresent
                        && statusPill.battery.type === UPowerDeviceType.Battery
                    readonly property real batPct:
                        statusPill.battery ? statusPill.battery.percentage * 100 : 0
                    readonly property bool batCharging:
                        statusPill.battery
                        && (statusPill.battery.state === UPowerDeviceState.Charging
                            || statusPill.battery.state === UPowerDeviceState.FullyCharged)

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

                  Row {
                      id: statusRow
                      anchors.centerIn: parent
                      spacing: 0

                      // ── System monitor segment (CPU%) ────────
                      Rectangle {
                          id: cpuSeg
                          width: cpuRow.implicitWidth + 16
                          height: statusPill.height
                          color: cpuHover.hovered
                              ? "${ca "base02" "cc"}"
                              : "transparent"
                          radius: 13
                          Behavior on color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: cpuHover }
                          Row {
                              id: cpuRow
                              anchors.centerIn: parent
                              spacing: 6
                              Text {
                                  anchors.verticalCenter: parent.verticalCenter
                                  text: "󰻠"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 13
                                  color: "${c "base0C"}"
                              }
                              Text {
                                  anchors.verticalCenter: parent.verticalCenter
                                  text: bar.cpuPct + "%"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                                  color: "${c "base05"}"
                              }
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: bar.systemMonitorRequested()
                          }
                      }

                      Rectangle {
                          width: 1; height: statusPill.height - 10
                          anchors.verticalCenter: parent.verticalCenter
                          color: "${ca "base03" "80"}"
                      }

                      // ── Volume segment ───────────────────────
                      Rectangle {
                          id: volSeg
                          width: volRow.implicitWidth + 16
                          height: statusPill.height
                          color: volHover.hovered
                              ? "${ca "base02" "cc"}"
                              : "transparent"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: volHover }
                          Row {
                              id: volRow
                              anchors.centerIn: parent
                              spacing: 6
                              Text {
                                  anchors.verticalCenter: parent.verticalCenter
                                  text: statusPill.volIcon(statusPill.volPct, statusPill.muted)
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 13
                                  color: statusPill.muted
                                      ? "${c "base08"}"
                                      : "${c "base0E"}"
                              }
                              Text {
                                  anchors.verticalCenter: parent.verticalCenter
                                  text: statusPill.muted
                                      ? "muted"
                                      : Math.round(statusPill.volPct) + "%"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                                  color: "${c "base05"}"
                              }
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton
                              onClicked: function(mouse) {
                                  if (mouse.button === Qt.MiddleButton) {
                                      if (statusPill.sink && statusPill.sink.audio)
                                          statusPill.sink.audio.muted = !statusPill.sink.audio.muted
                                  } else {
                                      bar.audioRequested()
                                  }
                              }
                              onWheel: function(wheel) {
                                  if (!statusPill.sink || !statusPill.sink.audio) return
                                  var step = wheel.angleDelta.y > 0 ? 0.05 : -0.05
                                  var v = Math.max(0, Math.min(1,
                                      statusPill.sink.audio.volume + step))
                                  statusPill.sink.audio.volume = v
                              }
                          }
                      }

                      Rectangle {
                          width: 1; height: statusPill.height - 10
                          anchors.verticalCenter: parent.verticalCenter
                          color: "${ca "base03" "80"}"
                      }

                      // ── Network segment ──────────────────────
                      Rectangle {
                          id: netSeg
                          width: netRow.implicitWidth + 16
                          height: statusPill.height
                          color: netHover.hovered
                              ? "${ca "base02" "cc"}"
                              : "transparent"
                          radius: 13
                          Behavior on color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: netHover }
                          Row {
                              id: netRow
                              anchors.centerIn: parent
                              spacing: 6
                              Text {
                                  anchors.verticalCenter: parent.verticalCenter
                                  text: bar.netState === "wired"
                                      ? "󰈀"
                                      : statusPill.wifiIcon(bar.netSignal)
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 13
                                  color: bar.netState === "off"
                                      ? "${c "base08"}"
                                      : "${c "base0B"}"
                              }
                              Text {
                                  visible: text !== ""
                                  anchors.verticalCenter: parent.verticalCenter
                                  text: bar.netState === "off"
                                      ? "offline"
                                      : (bar.netState === "wired"
                                          ? "wired"
                                          : (bar.netSsid.length > 12
                                              ? bar.netSsid.substring(0, 12) + "…"
                                              : bar.netSsid))
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                                  color: "${c "base05"}"
                                  elide: Text.ElideRight
                              }
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: bar.networkRequested()
                          }
                      }
                      ${lib.optionalString battery ''
                        Rectangle {
                            visible: statusPill.hasBattery
                            width: 1; height: statusPill.height - 10
                            anchors.verticalCenter: parent.verticalCenter
                            color: "${ca "base03" "80"}"
                        }

                        // ── Battery segment ──────────────────────
                        Rectangle {
                            id: batSeg
                            visible: statusPill.hasBattery
                            width: visible ? batRow.implicitWidth + 16 : 0
                            height: statusPill.height
                            color: batHover.hovered
                                ? "${ca "base02" "cc"}"
                                : "transparent"
                            radius: 13
                            Behavior on color { ColorAnimation { duration: 120 } }
                            HoverHandler { id: batHover }
                            Row {
                                id: batRow
                                anchors.centerIn: parent
                                spacing: 6
                                Text {
                                    anchors.verticalCenter: parent.verticalCenter
                                    text: statusPill.batteryIcon(
                                        statusPill.batPct,
                                        statusPill.batCharging)
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 13
                                    color: statusPill.batteryColor(
                                        statusPill.batPct,
                                        statusPill.batCharging)
                                }
                                Text {
                                    anchors.verticalCenter: parent.verticalCenter
                                    text: Math.round(statusPill.batPct) + "%"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 11
                                    color: "${c "base05"}"
                                }
                            }
                        }
                      ''}
                  }
              }

              // ── Wallpaper switcher ───────────────────────────
              Text {
                  visible: bar.isFocused
                  text: "󰸉"
                  font.family: "RobotoMono Nerd Font"
                  font.pixelSize: 14
                  color: "${c "base0E"}"
                  MouseArea {
                      anchors.fill: parent
                      cursorShape: Qt.PointingHandCursor
                      onClicked: bar.wallpaperRequested()
                  }
              }

              // ── Dashboard toggle ─────────────────────────────
              Text {
                  visible: bar.isFocused
                  text: "󰕮"
                  font.family: "RobotoMono Nerd Font"
                  font.pixelSize: 14
                  color: "${c "base0A"}"
                  MouseArea {
                      anchors.fill: parent
                      cursorShape: Qt.PointingHandCursor
                      onClicked: bar.dashboardRequested()
                  }
              }

              // ── Session / power menu ─────────────────────────
              Text {
                  visible: bar.isFocused
                  text: "󰐥"
                  font.family: "RobotoMono Nerd Font"
                  font.pixelSize: 14
                  color: "${c "base08"}"
                  MouseArea {
                      anchors.fill: parent
                      cursorShape: Qt.PointingHandCursor
                      onClicked: bar.sessionRequested()
                  }
              }
          }
      }
  }
''
