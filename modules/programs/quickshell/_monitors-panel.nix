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
          target: "monitors"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      property var    monitors: []
      property string primary:  ""
      property bool   busy:     false

      function refresh() { monitorsProc.running = true }

      Process {
          id: monitorsProc
          command: ["sh", "-c",
              "hyprctl monitors -j all 2>/dev/null || hyprctl monitors -j"]
          stdout: StdioCollector { id: monitorsOut }
          onExited: {
              var list = []
              try {
                  var arr = JSON.parse(monitorsOut.text || "[]")
                  for (var i = 0; i < arr.length; ++i) {
                      var m = arr[i]
                      list.push({
                          name:     m.name        || "",
                          desc:     m.description || "",
                          w:        m.width       || 0,
                          h:        m.height      || 0,
                          hz:       m.refreshRate || 0,
                          x:        m.x           || 0,
                          y:        m.y           || 0,
                          scale:    m.scale       || 1,
                          disabled: !!m.disabled,
                          mirrorOf: m.mirrorOf    || ""
                      })
                  }
              } catch (e) {}
              root.monitors = list

              // Keep / pick a sensible primary.
              var hasPrimary = false
              for (var j = 0; j < list.length; ++j)
                  if (list[j].name === root.primary && !list[j].disabled)
                      { hasPrimary = true; break }
              if (!hasPrimary) {
                  root.primary = ""
                  for (var k = 0; k < list.length; ++k) {
                      if (!list[k].disabled) { root.primary = list[k].name; break }
                  }
                  if (root.primary === "" && list.length > 0)
                      root.primary = list[0].name
              }
          }
      }

      Process {
          id: actionProc
          onExited: {
              root.busy = false
              root.refresh()
          }
      }

      function runCommands(cmds) {
          if (cmds.length === 0) return
          root.busy = true
          actionProc.command = ["sh", "-c", cmds.join(" && ")]
          actionProc.running = true
      }

      function applySideBySide() {
          if (root.monitors.length === 0 || root.primary === "") return
          var cmds = []
          cmds.push("hyprctl keyword monitor " + root.primary + ",preferred,0x0,1")
          for (var i = 0; i < root.monitors.length; ++i) {
              var m = root.monitors[i]
              if (m.name === root.primary) continue
              cmds.push("hyprctl keyword monitor " + m.name + ",preferred,auto-right,1")
          }
          runCommands(cmds)
      }

      function applyMirror() {
          if (root.monitors.length === 0 || root.primary === "") return
          var cmds = ["hyprctl keyword monitor " + root.primary + ",preferred,0x0,1"]
          for (var i = 0; i < root.monitors.length; ++i) {
              var m = root.monitors[i]
              if (m.name === root.primary) continue
              cmds.push("hyprctl keyword monitor " + m.name
                  + ",preferred,0x0,1,mirror," + root.primary)
          }
          runCommands(cmds)
      }

      function applyPrimaryOnly() {
          if (root.monitors.length === 0 || root.primary === "") return
          var cmds = ["hyprctl keyword monitor " + root.primary + ",preferred,0x0,1"]
          for (var i = 0; i < root.monitors.length; ++i) {
              var m = root.monitors[i]
              if (m.name === root.primary) continue
              cmds.push("hyprctl keyword monitor " + m.name + ",disable")
          }
          runCommands(cmds)
      }

      function toggleEnabled(name, currentlyDisabled) {
          if (name === "") return
          var cmds = []
          if (currentlyDisabled)
              cmds.push("hyprctl keyword monitor " + name + ",preferred,auto,1")
          else
              cmds.push("hyprctl keyword monitor " + name + ",disable")
          runCommands(cmds)
      }

      function currentMode() {
          if (root.monitors.length === 0) return "—"
          var enabled = []
          for (var i = 0; i < root.monitors.length; ++i)
              if (!root.monitors[i].disabled) enabled.push(root.monitors[i])
          if (enabled.length === 0) return "All off"
          if (enabled.length === 1) return "Single"
          for (var j = 0; j < enabled.length; ++j)
              if (enabled[j].mirrorOf && enabled[j].mirrorOf !== "")
                  return "Mirror"
          return "Extended"
      }

      Timer {
          running: root.opened
          repeat: true
          interval: 5000
          onTriggered: root.refresh()
      }

      PanelWindow {
          id: panel
          visible: root.opened || card.opacity > 0.01

          WlrLayershell.namespace: "quickshell-monitors"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

              anchors { top: true; bottom: true; left: true; right: true }
              color: "transparent"
          
              Shortcut { sequences: ["Escape"]; onActivated: root.hide() }
          
                  // Full-screen dismiss layer — stops at the bar so it never blocks it
                  MouseArea {
                      anchors.fill: parent
                      anchors.bottomMargin: 48
                      onClicked: root.hide()
                  }
          
              Rectangle {
                  id: card
                  width: 420
                  height: 540
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
                  anchors.fill: parent
                  anchors.margins: 14
                  spacing: 10

                  // ── Header ───────────────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      Text {
                          text: "󰍹  Displays"
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

                  // ── Current mode summary ─────────────────────
                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 44
                      color: "${ca "base01" "75"}"
                      radius: 10
                      border.width: 1
                      border.color: "${c "base02"}"

                      RowLayout {
                          anchors.fill: parent
                          anchors.margins: 12
                          spacing: 8
                          Text {
                              text: "Current mode"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                          }
                          Item { Layout.fillWidth: true }
                          Text {
                              text: root.currentMode()
                              color: "${c "base0D"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                              font.weight: Font.Medium
                          }
                      }
                  }

                  // ── Mode action buttons ──────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      spacing: 6

                      Repeater {
                          model: [
                              { label: "Extend", icon: "󰹑", action: "extend" },
                              { label: "Mirror", icon: "󰍺", action: "mirror" },
                              { label: "Single", icon: "󰍹", action: "single" }
                          ]
                          delegate: Rectangle {
                              required property var modelData
                              Layout.fillWidth: true
                              Layout.preferredHeight: 60
                              radius: 10
                              color: modeHover.hovered
                                  ? "${ca "base02" "cc"}"
                                  : "${ca "base01" "75"}"
                              Behavior on color { ColorAnimation { duration: 120 } }
                              border.width: 1
                              border.color: "${c "base02"}"
                              HoverHandler { id: modeHover }

                              ColumnLayout {
                                  anchors.centerIn: parent
                                  spacing: 2
                                  Text {
                                      Layout.alignment: Qt.AlignHCenter
                                      text: modelData.icon
                                      color: "${c "base0D"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 20
                                  }
                                  Text {
                                      Layout.alignment: Qt.AlignHCenter
                                      text: modelData.label
                                      color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 11
                                  }
                              }

                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  enabled: !root.busy && root.monitors.length > 0
                                  onClicked: {
                                      if (modelData.action === "extend") root.applySideBySide()
                                      else if (modelData.action === "mirror") root.applyMirror()
                                      else if (modelData.action === "single") root.applyPrimaryOnly()
                                  }
                              }
                          }
                      }
                  }

                  Text {
                      Layout.fillWidth: true
                      text: root.monitors.length > 1
                          ? "Tap a monitor to choose primary · icon toggles on/off"
                          : "Connect another display for mirror / extend"
                      color: "${c "base04"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 10
                      wrapMode: Text.Wrap
                  }

                  // ── Monitor list ─────────────────────────────
                  Rectangle {
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      color: "${ca "base01" "75"}"
                      radius: 10
                      border.width: 1
                      border.color: "${c "base02"}"

                      Text {
                          anchors.centerIn: parent
                          visible: root.monitors.length === 0
                          text: "No monitors detected"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 11
                      }

                      ListView {
                          id: monList
                          anchors.fill: parent
                          anchors.margins: 6
                          visible: root.monitors.length > 0
                          clip: true
                          spacing: 4
                          model: root.monitors
                          boundsBehavior: Flickable.StopAtBounds
                          ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

                          delegate: Rectangle {
                              required property var modelData
                              width: monList.width
                              height: 64
                              radius: 8
                              color: rowHover.hovered
                                  ? "${ca "base02" "aa"}"
                                  : "${ca "base00" "80"}"
                              Behavior on color { ColorAnimation { duration: 120 } }
                              border.width: 1
                              border.color: modelData.name === root.primary
                                  ? "${c "base0D"}"
                                  : "${c "base02"}"
                              HoverHandler { id: rowHover }

                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: root.primary = modelData.name
                              }

                              RowLayout {
                                  anchors.fill: parent
                                  anchors.leftMargin: 10
                                  anchors.rightMargin: 10
                                  spacing: 10

                                  // primary radio dot
                                  Rectangle {
                                      Layout.preferredWidth: 14
                                      Layout.preferredHeight: 14
                                      radius: 7
                                      border.width: 2
                                      border.color: modelData.name === root.primary
                                          ? "${c "base0D"}"
                                          : "${c "base04"}"
                                      color: modelData.name === root.primary
                                          ? "${c "base0D"}"
                                          : "transparent"
                                  }

                                  ColumnLayout {
                                      Layout.fillWidth: true
                                      spacing: 1
                                      Text {
                                          Layout.fillWidth: true
                                          text: modelData.name
                                              + (modelData.disabled ? "  (off)" : "")
                                          color: "${c "base05"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 12
                                          font.weight: Font.Medium
                                          elide: Text.ElideRight
                                      }
                                      Text {
                                          Layout.fillWidth: true
                                          text: modelData.disabled
                                              ? "disabled"
                                              : (modelData.mirrorOf && modelData.mirrorOf !== ""
                                                  ? ("mirror of " + modelData.mirrorOf)
                                                  : (modelData.w + "×" + modelData.h
                                                      + "@" + Math.round(modelData.hz) + "Hz  "
                                                      + "(" + modelData.x + "," + modelData.y + ")"))
                                          color: "${c "base04"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 10
                                          elide: Text.ElideRight
                                      }
                                  }

                                  Rectangle {
                                      Layout.preferredWidth: 32
                                      Layout.preferredHeight: 32
                                      radius: 6
                                      color: toggleHover.hovered
                                          ? "${ca "base02" "cc"}"
                                          : "transparent"
                                      Behavior on color { ColorAnimation { duration: 120 } }
                                      HoverHandler { id: toggleHover }

                                      Text {
                                          anchors.centerIn: parent
                                          text: modelData.disabled ? "󰍶" : "󰍹"
                                          color: modelData.disabled
                                              ? "${c "base08"}"
                                              : "${c "base0B"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 16
                                      }

                                      MouseArea {
                                          anchors.fill: parent
                                          cursorShape: Qt.PointingHandCursor
                                          enabled: !root.busy
                                          onClicked: root.toggleEnabled(
                                              modelData.name, modelData.disabled)
                                      }
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
