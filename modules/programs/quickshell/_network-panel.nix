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
      function show()   { root.opened = true; root.refresh() }
      function hide()   { root.opened = false }
      function toggle() { if (root.opened) root.hide(); else root.show() }

      IpcHandler {
          target: "network"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      property string activeSsid: ""
      property string activeType: "off"
      property bool   wifiEnabled: true
      property var    networks: []
      property bool   busy: false

      function refresh() {
          statusProc.running   = true
          wifiListProc.running = true
      }

      Process {
          id: statusProc
          command: ["sh", "-c",
              "wifi=$(nmcli -t -f WIFI g 2>/dev/null); " +
              "act=$(nmcli -t -f TYPE,STATE,CONNECTION device status 2>/dev/null | awk -F: '$2==\"connected\"{print $1\":\"$3; exit}'); " +
              "echo \"$wifi||$act\""]
          stdout: StdioCollector { id: statusOut }
          onExited: {
              var raw   = (statusOut.text || "").trim()
              var parts = raw.split("||")
              root.wifiEnabled =
                  (parts[0] || "").toLowerCase().indexOf("enabled") !== -1
              var act = parts[1] || ""
              if (act.indexOf("wifi:") === 0) {
                  root.activeType = "wifi"
                  root.activeSsid = act.substring(5)
              } else if (act.indexOf("ethernet:") === 0) {
                  root.activeType = "wired"
                  root.activeSsid = act.substring(9)
              } else {
                  root.activeType = "off"
                  root.activeSsid = ""
              }
          }
      }

      Process {
          id: wifiListProc
          command: ["sh", "-c",
              "nmcli -t -f IN-USE,SSID,SIGNAL,SECURITY device wifi list 2>/dev/null | head -n 30"]
          stdout: StdioCollector { id: wifiListOut }
          onExited: {
              var lines = (wifiListOut.text || "").trim().split("\n")
              var arr = []
              for (var i = 0; i < lines.length; ++i) {
                  var raw = lines[i]
                  if (!raw) continue
                  var safe  = raw.replace(/\\:/g, "\u0001")
                  var parts = safe.split(":")
                  if (parts.length < 4) continue
                  var ssid = (parts[1] || "").replace(/\u0001/g, ":")
                  if (ssid === "") continue
                  arr.push({
                      inUse:  (parts[0] || "") === "*",
                      ssid:   ssid,
                      signal: parseInt(parts[2] || "0") || 0,
                      secure: (parts[3] || "") !== ""
                  })
              }
              arr.sort(function(a, b) { return b.signal - a.signal })
              var seen = ({})
              var unique = []
              for (var j = 0; j < arr.length; ++j) {
                  if (seen[arr[j].ssid]) continue
                  seen[arr[j].ssid] = true
                  unique.push(arr[j])
              }
              root.networks = unique
          }
      }

      Process {
          id: actionProc
          onExited: {
              root.busy = false
              root.refresh()
          }
      }

      function toggleWifi() {
          root.busy = true
          actionProc.command = ["sh", "-c",
              "nmcli radio wifi " + (root.wifiEnabled ? "off" : "on")]
          actionProc.running = true
      }
      function connectTo(ssid) {
          root.busy = true
          actionProc.command = ["sh", "-c",
              "nmcli device wifi connect " + JSON.stringify(ssid) + " || true"]
          actionProc.running = true
      }
      function disconnectActive() {
          if (root.activeSsid === "") return
          root.busy = true
          actionProc.command = ["sh", "-c",
              "nmcli connection down id " + JSON.stringify(root.activeSsid) + " || true"]
          actionProc.running = true
      }

      Timer {
          running: root.opened
          repeat: true
          interval: 5000
          onTriggered: root.refresh()
      }

      function wifiIcon(s) {
          if (s >= 75) return "󰤨"
          if (s >= 50) return "󰤥"
          if (s >= 25) return "󰤢"
          if (s > 0)   return "󰤟"
          return "󰤮"
      }

      PanelWindow {
          id: panel
          visible: root.opened || card.opacity > 0.01

          WlrLayershell.namespace: "quickshell-network"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors { top: true; right: true }
          margins { top: 50; right: 8 }
          implicitWidth: 380
          implicitHeight: 460
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
                  anchors.fill: parent
                  anchors.margins: 14
                  spacing: 10

                  RowLayout {
                      Layout.fillWidth: true
                      Text {
                          text: "󰖩  Network"
                          color: "${c "base0B"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 14
                          font.weight: Font.Medium
                      }
                      Item { Layout.fillWidth: true }
                      Text {
                          text: root.busy ? "…" : "󰑐"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 14
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: root.refresh()
                          }
                      }
                  }

                  // ── Status / current connection ──────────────
                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 56
                      color: "${ca "base01" "75"}"
                      radius: 10
                      border.width: 1
                      border.color: "${c "base02"}"

                      RowLayout {
                          anchors.fill: parent
                          anchors.margins: 12
                          spacing: 10

                          Text {
                              text: root.activeType === "wired"
                                  ? "󰈀"
                                  : (root.activeType === "wifi"
                                      ? root.wifiIcon(80)
                                      : "󰤮")
                              color: root.activeType === "off"
                                  ? "${c "base08"}"
                                  : "${c "base0B"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 22
                          }

                          ColumnLayout {
                              Layout.fillWidth: true
                              spacing: 0
                              Text {
                                  text: root.activeType === "off"
                                      ? "Disconnected"
                                      : (root.activeType === "wired"
                                          ? "Wired"
                                          : "Connected")
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              Text {
                                  Layout.fillWidth: true
                                  text: root.activeSsid !== "" ? root.activeSsid : "—"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 12
                                  elide: Text.ElideRight
                              }
                          }

                          Text {
                              visible: root.activeType !== "off"
                              text: "Disconnect"
                              color: "${c "base08"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 10
                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: root.disconnectActive()
                              }
                          }
                      }
                  }

                  // ── Wi-Fi toggle ─────────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      Text {
                          text: "Wi-Fi"
                          color: "${c "base05"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 12
                      }
                      Item { Layout.fillWidth: true }
                      Rectangle {
                          Layout.preferredWidth: 38
                          Layout.preferredHeight: 20
                          radius: 10
                          color: root.wifiEnabled
                              ? "${c "base0D"}"
                              : "${ca "base02" "cc"}"
                          Behavior on color { ColorAnimation { duration: 180 } }

                          Rectangle {
                              width: 16; height: 16; radius: 8
                              color: "${c "base05"}"
                              anchors.verticalCenter: parent.verticalCenter
                              x: root.wifiEnabled
                                  ? parent.width - width - 2
                                  : 2
                              Behavior on x {
                                  NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
                              }
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: root.toggleWifi()
                          }
                      }
                  }

                  // ── Network list ─────────────────────────────
                  Rectangle {
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      color: "${ca "base01" "75"}"
                      radius: 10
                      border.width: 1
                      border.color: "${c "base02"}"

                      ListView {
                          id: netList
                          anchors.fill: parent
                          anchors.margins: 6
                          clip: true
                          spacing: 2
                          model: root.networks
                          boundsBehavior: Flickable.StopAtBounds
                          ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

                          delegate: Rectangle {
                              required property var modelData
                              width: netList.width
                              height: 38
                              radius: 6
                              color: hover.hovered
                                  ? "${ca "base02" "aa"}"
                                  : "transparent"
                              Behavior on color { ColorAnimation { duration: 120 } }
                              HoverHandler { id: hover }

                              RowLayout {
                                  anchors.fill: parent
                                  anchors.leftMargin: 10
                                  anchors.rightMargin: 10
                                  spacing: 10

                                  Text {
                                      text: root.wifiIcon(modelData.signal)
                                      color: modelData.inUse
                                          ? "${c "base0B"}"
                                          : "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 16
                                  }
                                  Text {
                                      Layout.fillWidth: true
                                      text: modelData.ssid
                                      color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 12
                                      elide: Text.ElideRight
                                      font.weight: modelData.inUse
                                          ? Font.Medium
                                          : Font.Normal
                                  }
                                  Text {
                                      visible: modelData.secure
                                      text: "󰌾"
                                      color: "${c "base04"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 11
                                  }
                                  Text {
                                      text: modelData.signal + "%"
                                      color: "${c "base04"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 10
                                  }
                              }

                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: {
                                      if (!modelData.inUse)
                                          root.connectTo(modelData.ssid)
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
