{ c, ca }:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io
  import Quickshell.Services.UPower

  Scope {
      id: root

      // ── Lifecycle ───────────────────────────────────
      property bool opened: false
      function show()   { root.opened = true; root.refresh() }
      function hide()   { root.opened = false }
      function toggle() { if (root.opened) root.hide(); else root.show() }

      IpcHandler {
          target: "battery"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      // ── UPower battery state ────────────────────────
      readonly property var batDevice: UPower.displayDevice
      readonly property bool hasBat:
          batDevice && batDevice.isPresent
          && batDevice.type === UPowerDeviceType.Battery
      readonly property real batPct: hasBat ? batDevice.percentage * 100 : 0
      readonly property bool batCharging:
          hasBat && (batDevice.state === UPowerDeviceState.Charging
                  || batDevice.state === UPowerDeviceState.FullyCharged)
      readonly property bool batDischarging:
          hasBat && batDevice.state === UPowerDeviceState.Discharging
      readonly property bool batFull:
          hasBat && batDevice.state === UPowerDeviceState.FullyCharged
      readonly property real batTimeToEmpty: hasBat ? batDevice.timeToEmpty : 0
      readonly property real batTimeToFull:  hasBat ? batDevice.timeToFull  : 0
      readonly property real batEnergyRate:  hasBat ? batDevice.energyRate  : 0
      readonly property real batEnergy:      hasBat ? batDevice.energy      : 0
      readonly property real batEnergyCap:   hasBat ? batDevice.energyCapacity : 0

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
      function fmtDuration(s) {
          if (!s || s <= 0) return "—"
          var sec = Math.round(s)
          var h = Math.floor(sec / 3600)
          var m = Math.floor((sec % 3600) / 60)
          if (h > 0) return h + "h " + m + "m"
          return m + "m"
      }

      // ── Brightness via brightnessctl ────────────────
      property int  brightnessPct: 0
      property bool hasBrightness: false
      // Whether the user is currently dragging the slider —
      // suppresses background polling overwriting the value.
      property bool brightnessDragging: false

      Process {
          id: brightGet
          command: ["sh", "-c", "brightnessctl -m 2>/dev/null"]
          stdout: StdioCollector { id: brightOut }
          onExited: {
              // brightnessctl -m format:
              //   device,class,current,percentage%,max
              var line = ((brightOut.text || "").trim().split("\n")[0]) || ""
              var parts = line.split(",")
              if (parts.length >= 4) {
                  var pct = parseInt((parts[3] || "0").replace("%", "")) || 0
                  if (!root.brightnessDragging) root.brightnessPct = pct
                  root.hasBrightness = true
              } else {
                  root.hasBrightness = false
              }
          }
      }

      Process {
          id: brightSet
          command: ["sh", "-c", "true"]
      }

      function setBrightness(pct) {
          if (!root.hasBrightness) return
          var clamped = Math.max(1, Math.min(100, Math.round(pct)))
          root.brightnessPct = clamped
          brightSet.command = ["sh", "-c",
              "brightnessctl set " + clamped + "% >/dev/null 2>&1"]
          brightSet.running = true
      }

      function refresh() { brightGet.running = true }

      Timer {
          id: pollTimer
          running: root.opened
          repeat: true
          interval: 3000
          triggeredOnStart: true
          onTriggered: brightGet.running = true
      }

      // ── Panel window ────────────────────────────────
      PanelWindow {
          id: panel
          visible: root.opened || card.opacity > 0.01

          WlrLayershell.namespace: "quickshell-battery"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors { top: true; bottom: true; left: true; right: true }
          color: "transparent"

          Shortcut { sequences: ["Escape"]; onActivated: root.hide() }

          // Full-screen dismiss layer — stops above the bar
          MouseArea {
              anchors.fill: parent
              anchors.bottomMargin: 48
              onClicked: root.hide()
          }

          Rectangle {
              id: card
              width: 380
              height: contentCol.implicitHeight + 28
              anchors.bottom: parent.bottom
              anchors.right: parent.right
              anchors.bottomMargin: 52
              anchors.rightMargin: 8
              color: "${ca "base00" "ee"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"

              property real slideY: root.opened ? 0 : 18
              transform: Translate { y: card.slideY }
              transformOrigin: Item.Bottom
              opacity: root.opened ? 1 : 0
              scale: root.opened ? 1 : 0.96
              Behavior on opacity {
                  NumberAnimation { duration: 220; easing.type: Easing.OutCubic }
              }
              Behavior on scale {
                  NumberAnimation { duration: 280; easing.type: Easing.OutQuint }
              }
              Behavior on slideY {
                  NumberAnimation { duration: 280; easing.type: Easing.OutQuint }
              }

              ColumnLayout {
                  id: contentCol
                  anchors.fill: parent
                  anchors.margins: 14
                  spacing: 12

                  // ── Header ──────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      spacing: 10

                      Rectangle {
                          Layout.preferredWidth: 38
                          Layout.preferredHeight: 38
                          radius: 19
                          color: "${ca "base02" "66"}"
                          border.width: 1
                          border.color: "${ca "base02" "aa"}"
                          Text {
                              anchors.centerIn: parent
                              text: root.batteryIcon(root.batPct, root.batCharging)
                              color: root.batteryColor(root.batPct, root.batCharging)
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 20
                          }
                      }

                      ColumnLayout {
                          Layout.fillWidth: true
                          spacing: 0
                          Text {
                              text: "󱊥  Battery"
                              color: "${c "base0B"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 14
                              font.weight: Font.Medium
                          }
                          Text {
                              text: !root.hasBat
                                  ? "No battery"
                                  : (root.batFull
                                      ? "Fully charged"
                                      : (root.batCharging
                                          ? "Charging"
                                          : "On battery"))
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 10
                          }
                      }

                      Text {
                          text: root.hasBat ? Math.round(root.batPct) + "%" : "—"
                          color: "${c "base05"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 22
                          font.weight: Font.Medium
                      }
                  }

                  // ── Visual battery bar ──────────────
                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 10
                      radius: 5
                      color: "${ca "base02" "aa"}"
                      border.width: 1
                      border.color: "${c "base02"}"
                      visible: root.hasBat

                      Rectangle {
                          anchors.left: parent.left
                          anchors.top: parent.top
                          anchors.bottom: parent.bottom
                          anchors.margins: 1
                          width: Math.max(0, (parent.width - 2)
                              * Math.max(0, Math.min(1, root.batPct / 100)))
                          radius: 4
                          color: root.batteryColor(root.batPct, root.batCharging)
                          Behavior on width { NumberAnimation { duration: 240 } }
                          Behavior on color { ColorAnimation  { duration: 200 } }
                      }
                  }

                  // ── Info grid (2x2 mini cards) ──────
                  GridLayout {
                      Layout.fillWidth: true
                      columns: 2
                      rowSpacing: 6
                      columnSpacing: 6
                      visible: root.hasBat

                      // Time remaining / to full
                      Rectangle {
                          Layout.fillWidth: true
                          Layout.preferredHeight: 54
                          radius: 8
                          color: "${ca "base01" "75"}"
                          border.width: 1
                          border.color: "${c "base02"}"
                          ColumnLayout {
                              anchors.fill: parent
                              anchors.margins: 8
                              spacing: 1
                              Text {
                                  text: root.batCharging
                                      ? "󰂄  Time to full"
                                      : "󰁾  Time remaining"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              Text {
                                  text: root.batFull
                                      ? "—"
                                      : (root.batCharging
                                          ? root.fmtDuration(root.batTimeToFull)
                                          : root.fmtDuration(root.batTimeToEmpty))
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 14
                                  font.weight: Font.Medium
                              }
                          }
                      }

                      // Power rate
                      Rectangle {
                          Layout.fillWidth: true
                          Layout.preferredHeight: 54
                          radius: 8
                          color: "${ca "base01" "75"}"
                          border.width: 1
                          border.color: "${c "base02"}"
                          ColumnLayout {
                              anchors.fill: parent
                              anchors.margins: 8
                              spacing: 1
                              Text {
                                  text: root.batCharging
                                      ? "󱐋  Charge rate"
                                      : "󱐋  Power draw"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              Text {
                                  text: root.batEnergyRate > 0
                                      ? root.batEnergyRate.toFixed(1) + " W"
                                      : "—"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 14
                                  font.weight: Font.Medium
                              }
                          }
                      }

                      // Energy (Wh)
                      Rectangle {
                          Layout.fillWidth: true
                          Layout.preferredHeight: 54
                          radius: 8
                          color: "${ca "base01" "75"}"
                          border.width: 1
                          border.color: "${c "base02"}"
                          ColumnLayout {
                              anchors.fill: parent
                              anchors.margins: 8
                              spacing: 1
                              Text {
                                  text: "󱊣  Energy"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              Text {
                                  text: root.batEnergyCap > 0
                                      ? (root.batEnergy.toFixed(1) + " / "
                                          + root.batEnergyCap.toFixed(1) + " Wh")
                                      : "—"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 12
                                  font.weight: Font.Medium
                                  elide: Text.ElideRight
                              }
                          }
                      }

                      // State
                      Rectangle {
                          Layout.fillWidth: true
                          Layout.preferredHeight: 54
                          radius: 8
                          color: "${ca "base01" "75"}"
                          border.width: 1
                          border.color: "${c "base02"}"
                          ColumnLayout {
                              anchors.fill: parent
                              anchors.margins: 8
                              spacing: 1
                              Text {
                                  text: "󰂑  State"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              Text {
                                  text: root.batFull
                                      ? "Fully charged"
                                      : (root.batCharging
                                          ? "Charging"
                                          : "Discharging")
                                  color: root.batCharging
                                      ? "${c "base0B"}"
                                      : (root.batPct <= 15
                                          ? "${c "base08"}"
                                          : "${c "base05"}")
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 13
                                  font.weight: Font.Medium
                              }
                          }
                      }
                  }

                  // ── Separator ───────────────────────
                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 1
                      color: "${ca "base02" "80"}"
                      visible: root.hasBrightness
                  }

                  // ── Brightness ──────────────────────
                  ColumnLayout {
                      Layout.fillWidth: true
                      visible: root.hasBrightness
                      spacing: 6

                      RowLayout {
                          Layout.fillWidth: true
                          spacing: 10

                          Rectangle {
                              Layout.preferredWidth: 30
                              Layout.preferredHeight: 30
                              radius: 15
                              color: "transparent"
                              Text {
                                  anchors.centerIn: parent
                                  text: root.brightnessPct >= 67
                                      ? "󰃝"
                                      : (root.brightnessPct >= 34 ? "󰃟" : "󰃞")
                                  color: "${c "base0A"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 18
                              }
                          }

                          ColumnLayout {
                              Layout.fillWidth: true
                              spacing: 0
                              Text {
                                  text: "Brightness"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              Text {
                                  Layout.fillWidth: true
                                  text: "Screen backlight"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                                  elide: Text.ElideRight
                              }
                          }

                          Text {
                              text: root.brightnessPct + "%"
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                          }
                      }

                      Slider {
                          id: brightSlider
                          Layout.fillWidth: true
                          from: 1; to: 100; stepSize: 1
                          value: root.brightnessPct
                          onPressedChanged: root.brightnessDragging = pressed
                          onMoved: root.setBrightness(value)
                      }
                  }

                  // ── No-battery fallback message ─────
                  Text {
                      Layout.fillWidth: true
                      visible: !root.hasBat
                      horizontalAlignment: Text.AlignHCenter
                      text: "No battery detected"
                      color: "${c "base04"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 11
                  }
              }
          }
      }
  }
''
