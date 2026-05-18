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

      // ── Workspace clients (for hover preview) ────────────────────
      property var clientsByWs: ({})
      property var hoveredDockItem: null
      property int hoveredWsId: -1

      Process {
          id: clientsProc
          command: ["sh", "-c", "hyprctl clients -j 2>/dev/null"]
          stdout: StdioCollector { id: clientsOut }
          onExited: {
              try {
                  var arr = JSON.parse(clientsOut.text || "[]")
                  var map = {}
                  for (var i = 0; i < arr.length; ++i) {
                      var c = arr[i]
                      var ws = c.workspace ? c.workspace.id : 0
                      if (!map[ws]) map[ws] = []
                      map[ws].push({
                          title: c.title || "",
                          cls:   c.class || "",
                          pinned: !!c.pinned,
                          floating: !!c.floating,
                      })
                  }
                  bar.clientsByWs = map
              } catch (e) {}
          }
      }
      Timer {
          id: clientsTimer
          running: true
          repeat: true
          interval: 3000
          triggeredOnStart: true
          onTriggered: clientsProc.running = true
      }

      // Hide-preview debounce: avoid flicker when moving between items
      Timer {
          id: previewHideTimer
          interval: 120
          repeat: false
          onTriggered: {
              if (bar.hoveredDockItem === null) bar.hoveredWsId = -1
          }
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

              // ── Workspace switcher (styled as Win11 dock) ────────
              // Workspaces: 1=Browser, 2=Coding, 3=Terminal, 5=VM, 8=Slack, 9=Discord
              Row {
                  id: dockRow
                  anchors.verticalCenter: parent.verticalCenter
                  spacing: 2

                  Repeater {
                      model: [
                          { wsId: 1, icon: "󰈹", label: "Browser"         },
                          { wsId: 2, icon: "󰅴", label: "Coding"          },
                          { wsId: 3, icon: "󰆍", label: "Terminal"        },
                          { wsId: 5, icon: "󰢹", label: "Virtual Machine" },
                          { wsId: 8, icon: "󰒱", label: "Slack"           },
                          { wsId: 9, icon: "󰙯", label: "Discord"         }
                      ]

                      Item {
                          id: dockItem
                          required property var modelData
                          required property int index
                          width: 44
                          height: 44

                          property bool hov: dockItemHover.hovered
                          property bool isActive: Hyprland.focusedWorkspace
                              && Hyprland.focusedWorkspace.id === modelData.wsId
                          property bool hasWindows: {
                              var list = Hyprland.workspaces
                                  ? Hyprland.workspaces.values
                                  : []
                              for (var i = 0; i < list.length; ++i) {
                                  if (list[i] && list[i].id === dockItem.modelData.wsId)
                                      return true
                              }
                              return false
                          }

                          Rectangle {
                              id: dockItemBg
                              anchors.centerIn: parent
                              width: (dockItem.hov || dockItem.isActive) ? 38 : 34
                              height: (dockItem.hov || dockItem.isActive) ? 38 : 34
                              radius: 8
                              color: dockItem.isActive
                                  ? "${ca "base0D" "33"}"
                                  : (dockItem.hov
                                      ? "${ca "base02" "cc"}"
                                      : "transparent")
                              border.width: dockItem.isActive ? 1 : 0
                              border.color: "${ca "base0D" "88"}"
                              Behavior on width  { NumberAnimation { duration: 150; easing.type: Easing.OutQuint } }
                              Behavior on height { NumberAnimation { duration: 150; easing.type: Easing.OutQuint } }
                              Behavior on color  { ColorAnimation  { duration: 120 } }

                              // Vertical offset (lift on hover)
                              property real offsetY: dockItem.hov ? -3 : 0
                              transform: Translate { y: dockItemBg.offsetY }
                              Behavior on offsetY {
                                  NumberAnimation { duration: 150; easing.type: Easing.OutQuint }
                              }

                              HoverHandler {
                                  id: dockItemHover
                                  onHoveredChanged: {
                                      if (hovered) {
                                          bar.hoveredDockItem = dockItem
                                          bar.hoveredWsId     = dockItem.modelData.wsId
                                          previewHideTimer.stop()
                                          clientsProc.running = true
                                      } else if (bar.hoveredDockItem === dockItem) {
                                          bar.hoveredDockItem = null
                                          previewHideTimer.restart()
                                      }
                                  }
                              }

                              Text {
                                  anchors.centerIn: parent
                                  text: dockItem.modelData.icon
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 20
                                  color: dockItem.isActive
                                      ? "${c "base0D"}"
                                      : (dockItem.hov
                                          ? "${c "base05"}"
                                          : (dockItem.hasWindows
                                              ? "${c "base05"}"
                                              : "${c "base04"}"))
                                  Behavior on color { ColorAnimation { duration: 120 } }
                              }
                          }

                          // Win11-style running/active indicator (underline)
                          // - Active workspace  → wide accent pill
                          // - Has windows       → small accent dot
                          // - Empty + inactive  → hidden
                          Rectangle {
                              id: runDot
                              anchors.horizontalCenter: parent.horizontalCenter
                              anchors.bottom: parent.bottom
                              anchors.bottomMargin: 2
                              width: dockItem.isActive
                                  ? 14
                                  : (dockItem.hasWindows ? 4 : 0)
                              height: 3
                              radius: 1.5
                              color: "${c "base0D"}"
                              visible: dockItem.isActive || dockItem.hasWindows
                              opacity: dockItem.isActive ? 1.0 : 0.75
                              Behavior on width {
                                  NumberAnimation { duration: 200; easing.type: Easing.OutQuint }
                              }
                              Behavior on opacity { NumberAnimation { duration: 150 } }
                          }

                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: Hyprland.dispatch(
                                  "hl.dsp.focus({ workspace = "
                                  + dockItem.modelData.wsId + " })")
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

      // ══════════════════════════════════════════════════════════════
      // Workspace hover preview (Win11-style thumbnail card)
      // ══════════════════════════════════════════════════════════════
      PopupWindow {
          id: wsPreview
          parentWindow: bar
          visible: bar.hoveredWsId > 0

          // Width/height of the preview card
          implicitWidth:  260
          implicitHeight: previewCard.implicitHeight + 4

          // Anchor centered above the hovered dock item.
          // anchor.rect uses coordinates relative to parentWindow.
          anchor.window: bar
          anchor.rect.x: bar.hoveredDockItem
              ? bar.hoveredDockItem.mapToItem(null,
                    bar.hoveredDockItem.width / 2, 0).x - wsPreview.implicitWidth / 2
              : 0
          anchor.rect.y: -wsPreview.implicitHeight - 6
          anchor.rect.width: wsPreview.implicitWidth
          anchor.rect.height: wsPreview.implicitHeight

          color: "transparent"

          // Resolve hovered ws metadata
          readonly property var hoveredMeta: {
              switch (bar.hoveredWsId) {
                  case 1: return { icon: "󰈹", label: "Browser"         }
                  case 2: return { icon: "󰅴", label: "Coding"          }
                  case 3: return { icon: "󰆍", label: "Terminal"        }
                  case 5: return { icon: "󰢹", label: "Virtual Machine" }
                  case 8: return { icon: "󰒱", label: "Slack"           }
                  case 9: return { icon: "󰙯", label: "Discord"         }
                  default: return { icon: "󰧞", label: "Workspace" }
              }
          }
          readonly property var clients:
              bar.clientsByWs[bar.hoveredWsId] || []

          Rectangle {
              id: previewCard
              anchors.fill: parent
              anchors.margins: 2
              color: "${ca "base00" "ee"}"
              radius: 12
              border.width: 1
              border.color: "${c "base02"}"

              implicitHeight: previewCol.implicitHeight + 20

              ColumnLayout {
                  id: previewCol
                  anchors.fill: parent
                  anchors.margins: 10
                  spacing: 6

                  // Header: icon + workspace label + count
                  RowLayout {
                      Layout.fillWidth: true
                      spacing: 8

                      Text {
                          text: wsPreview.hoveredMeta.icon
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 18
                          color: "${c "base0D"}"
                      }
                      ColumnLayout {
                          Layout.fillWidth: true
                          spacing: 0
                          Text {
                              text: wsPreview.hoveredMeta.label
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                              font.weight: Font.Medium
                              elide: Text.ElideRight
                          }
                          Text {
                              text: "Workspace " + bar.hoveredWsId
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 9
                          }
                      }
                      Rectangle {
                          Layout.alignment: Qt.AlignVCenter
                          width: countText.implicitWidth + 12
                          height: 16
                          radius: 8
                          color: wsPreview.clients.length > 0
                              ? "${ca "base0D" "44"}"
                              : "${ca "base02" "88"}"
                          Text {
                              id: countText
                              anchors.centerIn: parent
                              text: wsPreview.clients.length + " "
                                  + (wsPreview.clients.length === 1 ? "window" : "windows")
                              color: wsPreview.clients.length > 0
                                  ? "${c "base0D"}"
                                  : "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 9
                          }
                      }
                  }

                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 1
                      color: "${ca "base02" "80"}"
                  }

                  // Empty state
                  Text {
                      Layout.fillWidth: true
                      visible: wsPreview.clients.length === 0
                      text: "Empty workspace"
                      color: "${c "base04"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 10
                      horizontalAlignment: Text.AlignHCenter
                  }

                  // Window list
                  ColumnLayout {
                      Layout.fillWidth: true
                      spacing: 3
                      visible: wsPreview.clients.length > 0

                      Repeater {
                          model: wsPreview.clients.slice(0, 6)
                          RowLayout {
                              Layout.fillWidth: true
                              spacing: 6
                              required property var modelData

                              Rectangle {
                                  width: 4
                                  height: 4
                                  radius: 2
                                  color: "${c "base0D"}"
                                  Layout.alignment: Qt.AlignVCenter
                              }
                              Text {
                                  Layout.fillWidth: true
                                  text: (modelData.title && modelData.title.length > 0)
                                      ? modelData.title
                                      : (modelData.cls || "—")
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                                  elide: Text.ElideRight
                              }
                              Text {
                                  visible: modelData.cls
                                      && modelData.cls.length > 0
                                  text: modelData.cls
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 9
                                  elide: Text.ElideRight
                                  Layout.maximumWidth: 80
                              }
                          }
                      }

                      // Overflow indicator
                      Text {
                          Layout.fillWidth: true
                          visible: wsPreview.clients.length > 6
                          text: "+ " + (wsPreview.clients.length - 6) + " more…"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 9
                          horizontalAlignment: Text.AlignHCenter
                      }
                  }
              }
          }
      }
  }
''
