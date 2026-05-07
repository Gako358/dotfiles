{ c, ca }:
''
  import QtQuick
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io
  import Quickshell.Services.SystemTray

  Scope {
      id: root

      property bool opened: false
      function show()   { root.opened = true }
      function hide()   { root.opened = false }
      function toggle() { root.opened = !root.opened }

      IpcHandler {
          target: "tray"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      PanelWindow {
          id: panel
          visible: root.opened || card.opacity > 0.01

          WlrLayershell.namespace: "quickshell-tray"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors { top: true; right: true }
          margins { top: 50; right: 8 }
          implicitWidth: 320
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
                  spacing: 10

                  RowLayout {
                      Layout.fillWidth: true
                      spacing: 8
                      Text {
                          text: "󰂩  System Tray"
                          color: "${c "base0D"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 14
                          font.weight: Font.Medium
                      }
                      Item { Layout.fillWidth: true }
                      Text {
                          text: SystemTray.items
                              ? SystemTray.items.values.length + " item"
                                  + (SystemTray.items.values.length === 1 ? "" : "s")
                              : "0 items"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 11
                      }
                  }

                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 1
                      color: "${ca "base02" "80"}"
                  }

                  // ── Empty state ──────────────────────────────
                  Text {
                      visible: !SystemTray.items
                          || SystemTray.items.values.length === 0
                      Layout.fillWidth: true
                      Layout.topMargin: 12
                      Layout.bottomMargin: 12
                      horizontalAlignment: Text.AlignHCenter
                      text: "No tray items"
                      color: "${c "base04"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 12
                  }

                  // ── Tray items list ──────────────────────────
                  ColumnLayout {
                      Layout.fillWidth: true
                      spacing: 4

                      Repeater {
                          model: SystemTray.items
                          delegate: Rectangle {
                              required property SystemTrayItem modelData
                              id: itemRow

                              Layout.fillWidth: true
                              Layout.preferredHeight: 38
                              radius: 10
                              color: itemHover.hovered
                                  ? "${ca "base02" "cc"}"
                                  : "transparent"
                              Behavior on color { ColorAnimation { duration: 120 } }
                              HoverHandler { id: itemHover }

                              RowLayout {
                                  anchors.fill: parent
                                  anchors.leftMargin: 10
                                  anchors.rightMargin: 10
                                  spacing: 10

                                  Image {
                                      Layout.preferredWidth: 22
                                      Layout.preferredHeight: 22
                                      source: itemRow.modelData.icon
                                      fillMode: Image.PreserveAspectFit
                                      sourceSize.width: 22
                                      sourceSize.height: 22
                                  }

                                  ColumnLayout {
                                      Layout.fillWidth: true
                                      spacing: 0
                                      Text {
                                          Layout.fillWidth: true
                                          text: itemRow.modelData.title
                                              || itemRow.modelData.id
                                              || "—"
                                          color: "${c "base05"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 12
                                          elide: Text.ElideRight
                                      }
                                      Text {
                                          Layout.fillWidth: true
                                          visible: text !== ""
                                          text: itemRow.modelData.tooltipTitle || ""
                                          color: "${c "base04"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 10
                                          elide: Text.ElideRight
                                      }
                                  }

                                  // Right-click hint (menu)
                                  Text {
                                      visible: itemRow.modelData.hasMenu
                                      text: "󰍝"
                                      color: "${c "base04"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 12
                                  }
                              }

                              MouseArea {
                                  anchors.fill: parent
                                  acceptedButtons:
                                      Qt.LeftButton | Qt.RightButton | Qt.MiddleButton
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: function(mouse) {
                                      if (mouse.button === Qt.RightButton) {
                                          itemRow.modelData.secondaryActivate();
                                      } else if (mouse.button === Qt.MiddleButton) {
                                          itemRow.modelData.secondaryActivate();
                                      } else {
                                          itemRow.modelData.activate();
                                      }
                                      root.hide();
                                  }
                                  onWheel: function(wheel) {
                                      itemRow.modelData.scroll(
                                          wheel.angleDelta.y, false);
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
