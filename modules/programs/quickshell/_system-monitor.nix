{ c, ca }:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io

  Scope {
      id: root

      property bool opened: false
      function show()   { root.opened = true }
      function hide()   { root.opened = false }
      function toggle() { root.opened = !root.opened }

      IpcHandler {
          target: "sysmon"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      property real cpuPct: 0
      property real memPct: 0
      property string memInfo: "—"
      property real swapPct: 0
      property string swapInfo: "—"
      property string uptime: "—"
      property var procs: []

      Process {
          id: cpuProc
          command: ["sh", "-c",
              "top -bn1 | awk '/Cpu/ { printf \"%.0f\", 100 - $8 }'"]
          stdout: StdioCollector { id: cpuOut }
          onExited: root.cpuPct =
              parseFloat((cpuOut.text || "").trim() || "0") || 0
      }
      Process {
          id: memProc
          command: ["sh", "-c",
              "free -b | awk '" +
              "/^Mem:/  { printf \"%.1f|%.1f|%.1f\", $3/1073741824, $2/1073741824, ($3/$2)*100 } " +
              "/^Swap:/ { if ($2>0) printf \"|%.1f|%.1f|%.1f\", $3/1073741824, $2/1073741824, ($3/$2)*100; else printf \"|0|0|0\" }'"]
          stdout: StdioCollector { id: memOut }
          onExited: {
              var p = ((memOut.text || "").trim()).split("|")
              if (p.length >= 6) {
                  root.memInfo  = p[0] + " / " + p[1] + " GiB"
                  root.memPct   = parseFloat(p[2]) || 0
                  root.swapInfo = p[3] + " / " + p[4] + " GiB"
                  root.swapPct  = parseFloat(p[5]) || 0
              }
          }
      }
      Process {
          id: upProc
          command: ["sh", "-c",
              "awk '{u=int($1); d=int(u/86400); h=int((u%86400)/3600); m=int((u%3600)/60); s=\"\"; if(d>0) s=s d\"d \"; if(h>0) s=s h\"h \"; s=s m\"m\"; print s}' /proc/uptime"]
          stdout: StdioCollector { id: upOut }
          onExited: root.uptime = (upOut.text || "").trim() || "—"
      }
      Process {
          id: psProc
          command: ["sh", "-c",
              "ps -eo comm,%cpu,%mem --sort=-%cpu --no-headers | head -n 5"]
          stdout: StdioCollector { id: psOut }
          onExited: {
              var lines = (psOut.text || "").trim().split("\n")
              var arr = []
              for (var i = 0; i < lines.length; ++i) {
                  var f = lines[i].trim().split(/\s+/)
                  if (f.length < 3) continue
                  arr.push({
                      name: f[0],
                      cpu:  parseFloat(f[1]) || 0,
                      mem:  parseFloat(f[2]) || 0
                  })
              }
              root.procs = arr
          }
      }

      Timer {
          running: root.opened
          repeat: true
          interval: 2000
          triggeredOnStart: true
          onTriggered: {
              cpuProc.running = true
              memProc.running = true
              upProc.running  = true
              psProc.running  = true
          }
      }

      PanelWindow {
          id: panel
          visible: root.opened || card.opacity > 0.01

          WlrLayershell.namespace: "quickshell-sysmon"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors { top: true; right: true }
          margins { top: 50; right: 8 }
          implicitWidth: 380
          implicitHeight: contentCol.implicitHeight + 28
          color: "transparent"

          Shortcut { sequences: ["Escape"]; onActivated: root.hide() }

          Rectangle {
              id: card
              anchors.fill: parent
              color: "${ca "base00" "ee"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"

              property real slideY: root.opened ? 0 : -18
              transform: Translate { y: card.slideY }

              transformOrigin: Item.Top
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

                  Text {
                      text: "󰍛  System Monitor"
                      color: "${c "base0D"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 14
                      font.weight: Font.Medium
                  }

                  // ── CPU ──────────────────────────────────────
                  ColumnLayout {
                      Layout.fillWidth: true
                      spacing: 4
                      RowLayout {
                          Layout.fillWidth: true
                          Text {
                              text: "󰻠  CPU"
                              color: "${c "base0C"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                          }
                          Item { Layout.fillWidth: true }
                          Text {
                              text: Math.round(root.cpuPct) + "%"
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                          }
                      }
                      Rectangle {
                          Layout.fillWidth: true
                          Layout.preferredHeight: 6
                          radius: 3
                          color: "${ca "base02" "80"}"
                          Rectangle {
                              anchors.left: parent.left
                              anchors.top: parent.top
                              anchors.bottom: parent.bottom
                              width: parent.width * Math.min(1, root.cpuPct / 100)
                              radius: 3
                              color: "${c "base0C"}"
                              Behavior on width {
                                  NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                              }
                          }
                      }
                  }

                  // ── Memory ───────────────────────────────────
                  ColumnLayout {
                      Layout.fillWidth: true
                      spacing: 4
                      RowLayout {
                          Layout.fillWidth: true
                          Text {
                              text: "󰘚  Memory"
                              color: "${c "base0E"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                          }
                          Item { Layout.fillWidth: true }
                          Text {
                              text: root.memInfo
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                          }
                      }
                      Rectangle {
                          Layout.fillWidth: true
                          Layout.preferredHeight: 6
                          radius: 3
                          color: "${ca "base02" "80"}"
                          Rectangle {
                              anchors.left: parent.left
                              anchors.top: parent.top
                              anchors.bottom: parent.bottom
                              width: parent.width * Math.min(1, root.memPct / 100)
                              radius: 3
                              color: "${c "base0E"}"
                              Behavior on width {
                                  NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                              }
                          }
                      }
                  }

                  // ── Swap ─────────────────────────────────────
                  ColumnLayout {
                      Layout.fillWidth: true
                      spacing: 4
                      RowLayout {
                          Layout.fillWidth: true
                          Text {
                              text: "󰓡  Swap"
                              color: "${c "base09"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                          }
                          Item { Layout.fillWidth: true }
                          Text {
                              text: root.swapInfo
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                          }
                      }
                      Rectangle {
                          Layout.fillWidth: true
                          Layout.preferredHeight: 6
                          radius: 3
                          color: "${ca "base02" "80"}"
                          Rectangle {
                              anchors.left: parent.left
                              anchors.top: parent.top
                              anchors.bottom: parent.bottom
                              width: parent.width * Math.min(1, root.swapPct / 100)
                              radius: 3
                              color: "${c "base09"}"
                              Behavior on width {
                                  NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                              }
                          }
                      }
                  }

                  // ── Uptime ───────────────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      Text {
                          text: "󰅐  Uptime"
                          color: "${c "base0A"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 12
                      }
                      Item { Layout.fillWidth: true }
                      Text {
                          text: root.uptime
                          color: "${c "base05"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 12
                      }
                  }

                  // ── Top processes ────────────────────────────
                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: procsCol.implicitHeight + 16
                      color: "${ca "base01" "75"}"
                      radius: 10
                      border.width: 1
                      border.color: "${c "base02"}"

                      ColumnLayout {
                          id: procsCol
                          anchors.fill: parent
                          anchors.margins: 8
                          spacing: 4

                          Text {
                              text: "Top processes"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 10
                          }

                          Repeater {
                              model: root.procs
                              RowLayout {
                                  Layout.fillWidth: true
                                  spacing: 8
                                  Text {
                                      Layout.fillWidth: true
                                      text: modelData.name
                                      color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 11
                                      elide: Text.ElideRight
                                  }
                                  Text {
                                      text: modelData.cpu.toFixed(1) + "%"
                                      color: "${c "base0C"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 11
                                  }
                                  Text {
                                      text: modelData.mem.toFixed(1) + "%"
                                      color: "${c "base0E"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 11
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }
''
