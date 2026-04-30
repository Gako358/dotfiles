{ c, ca }:
''
  import QtQuick
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io

  Scope {
      id: root

      property var lockComponent: null
      property bool opened: false

      function show()   { root.opened = true }
      function hide()   { root.opened = false }
      function toggle() { root.opened = !root.opened }

      IpcHandler {
          target: "session"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      function run(cmd) {
          Quickshell.execDetached(cmd)
          root.hide()
      }

      function doLock() {
          if (root.lockComponent) root.lockComponent.lock()
          root.hide()
      }
      function doSuspend()  { run(["systemctl", "suspend"]) }
      function doLogout()   { run(["hyprctl", "dispatch", "exit"]) }
      function doReboot()   { run(["systemctl", "reboot"]) }
      function doShutdown() { run(["systemctl", "poweroff"]) }

      PanelWindow {
          id: panel
          visible: root.opened || sessionCard.opacity > 0.01

          WlrLayershell.namespace: "quickshell-session"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors {
              top: true
              right: true
          }
          margins {
              top: 48
              right: 8
          }

          implicitWidth: 280
          implicitHeight: contentCol.implicitHeight + 28
          color: "transparent"

          Shortcut {
              sequences: ["Escape"]
              onActivated: root.hide()
          }

          Rectangle {
              id: sessionCard
              anchors.fill: parent
              color: "${ca "base00" "ee"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"

              property real slideY: root.opened ? 0 : -18
              transform: Translate { y: sessionCard.slideY }

              transformOrigin: Item.TopRight
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
                  spacing: 8

                  Text {
                      Layout.alignment: Qt.AlignHCenter
                      text: "Session"
                      color: "${c "base0D"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 14
                      font.weight: Font.Medium
                  }

                  Item { Layout.preferredHeight: 4 }

                  Repeater {
                      model: [
                          { icon: "󰌾", label: "Lock",     color: "${c "base0B"}", action: "lock"     },
                          { icon: "󰒲", label: "Suspend",  color: "${c "base0C"}", action: "suspend"  },
                          { icon: "󰍃", label: "Log out",  color: "${c "base0A"}", action: "logout"   },
                          { icon: "󰜉", label: "Reboot",   color: "${c "base09"}", action: "reboot"   },
                          { icon: "󰐥", label: "Shutdown", color: "${c "base08"}", action: "shutdown" }
                      ]

                      Rectangle {
                          required property var modelData
                          Layout.fillWidth: true
                          Layout.preferredHeight: 44
                          radius: 10
                          color: hover.hovered
                              ? "${ca "base02" "cc"}"
                              : "${ca "base01" "75"}"
                          border.width: 1
                          border.color: "${c "base02"}"

                          Behavior on color {
                              ColorAnimation { duration: 120 }
                          }

                          HoverHandler { id: hover }

                          RowLayout {
                              anchors.fill: parent
                              anchors.leftMargin: 14
                              anchors.rightMargin: 14
                              spacing: 14

                              Text {
                                  text: modelData.icon
                                  color: modelData.color
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 20
                              }

                              Text {
                                  Layout.fillWidth: true
                                  text: modelData.label
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 13
                                  font.weight: Font.Medium
                              }
                          }

                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: {
                                  switch (modelData.action) {
                                      case "lock":     root.doLock();     break
                                      case "suspend":  root.doSuspend();  break
                                      case "logout":   root.doLogout();   break
                                      case "reboot":   root.doReboot();   break
                                      case "shutdown": root.doShutdown(); break
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
