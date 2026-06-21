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

      property string pendingAction: ""
      property string pendingLabel: ""

      function show()   { root.opened = true }
      function hide()   { root.opened = false; root.cancelConfirm() }
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

      function requestConfirm(action, label) {
          root.pendingAction = action
          root.pendingLabel = label
      }
      function cancelConfirm() {
          root.pendingAction = ""
          root.pendingLabel = ""
      }
      function confirmAction() {
          var a = root.pendingAction
          root.cancelConfirm()
          switch (a) {
              // Terminate the login session (not the user): logind drives it
              // externally and stops user@UID.service only after the session
              // closes, avoiding the teardown deadlock that froze the next login.
              case "logout":   root.run(["sh", "-c", "loginctl terminate-session \"$(loginctl --no-legend list-sessions | awk '$6 == \"user\" { print $1; exit }')\""]); break
              case "reboot":   root.run(["systemctl", "reboot"]);   break
              case "shutdown": root.run(["systemctl", "poweroff"]); break
          }
      }

      PanelWindow {
          id: panel
          visible: root.opened || sessionCard.opacity > 0.01

          WlrLayershell.namespace: "quickshell-session"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

              anchors { top: true; bottom: true; left: true; right: true }
              color: "transparent"
          
              Shortcut {
                  sequences: ["Escape"]
                  onActivated: root.hide()
              }
          
                  // Full-screen dismiss layer — stops at the bar so it never blocks it
                  MouseArea {
                      anchors.fill: parent
                      anchors.bottomMargin: 48
                      onClicked: root.hide()
                  }
          
              Rectangle {
                  id: sessionCard
                  width: 280
                  height: (root.pendingAction === "" ? contentCol.implicitHeight : confirmCol.implicitHeight) + 28
                  anchors.bottom: parent.bottom
                  anchors.right: parent.right
                  anchors.bottomMargin: 52
                  anchors.rightMargin: 8
              color: "${ca "base00" "ee"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"

                            property real slideY: root.opened ? 0 : 18
                            transform: Translate { y: sessionCard.slideY }
                            transformOrigin: Item.BottomRight
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
                  visible: root.pendingAction === ""
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
                                      case "lock":     root.doLock();    break
                                      case "suspend":  root.doSuspend(); break
                                      case "logout":   root.requestConfirm("logout", "Log out");    break
                                      case "reboot":   root.requestConfirm("reboot", "Reboot");     break
                                      case "shutdown": root.requestConfirm("shutdown", "Shutdown"); break
                                  }
                              }
                          }
                      }
                  }
              }

              ColumnLayout {
                  id: confirmCol
                  visible: root.pendingAction !== ""
                  anchors.fill: parent
                  anchors.margins: 14
                  spacing: 8

                  Text {
                      Layout.alignment: Qt.AlignHCenter
                      text: root.pendingLabel
                      color: "${c "base0D"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 14
                      font.weight: Font.Medium
                  }

                  Text {
                      Layout.fillWidth: true
                      horizontalAlignment: Text.AlignHCenter
                      text: "Are you sure?"
                      color: "${c "base05"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 12
                  }

                  Item { Layout.preferredHeight: 4 }

                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 44
                      radius: 10
                      color: confirmHover.hovered ? "${ca "base08" "cc"}" : "${ca "base08" "55"}"
                      border.width: 1
                      border.color: "${c "base08"}"
                      Behavior on color { ColorAnimation { duration: 120 } }
                      HoverHandler { id: confirmHover }
                      RowLayout {
                          anchors.fill: parent
                          anchors.leftMargin: 14
                          anchors.rightMargin: 14
                          spacing: 14
                          Text {
                              text: "󰄬"
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 20
                          }
                          Text {
                              Layout.fillWidth: true
                              text: "Confirm"
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 13
                              font.weight: Font.Medium
                          }
                      }
                      MouseArea {
                          anchors.fill: parent
                          cursorShape: Qt.PointingHandCursor
                          onClicked: root.confirmAction()
                      }
                  }

                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 44
                      radius: 10
                      color: cancelHover.hovered ? "${ca "base02" "cc"}" : "${ca "base01" "75"}"
                      border.width: 1
                      border.color: "${c "base02"}"
                      Behavior on color { ColorAnimation { duration: 120 } }
                      HoverHandler { id: cancelHover }
                      RowLayout {
                          anchors.fill: parent
                          anchors.leftMargin: 14
                          anchors.rightMargin: 14
                          spacing: 14
                          Text {
                              text: "󰜺"
                              color: "${c "base0B"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 20
                          }
                          Text {
                              Layout.fillWidth: true
                              text: "Cancel"
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 13
                              font.weight: Font.Medium
                          }
                      }
                      MouseArea {
                          anchors.fill: parent
                          cursorShape: Qt.PointingHandCursor
                          onClicked: root.cancelConfirm()
                      }
                  }
              }
          }
      }
  }
''
