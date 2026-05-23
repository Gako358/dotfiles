{ c, ca }:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Widgets
  import Quickshell.Wayland
  import Quickshell.Io

  Scope {
      id: root

      property var notifications: null
      property bool opened: false

      function show()   { root.opened = true }
      function hide()   { root.opened = false }
      function toggle() { root.opened = !root.opened }

      IpcHandler {
          target: "notifications-panel"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      readonly property var histModel:
          root.notifications ? root.notifications.historyModel : null

      PanelWindow {
          id: panel
          visible: root.opened || notifCard.opacity > 0.01

          WlrLayershell.namespace: "quickshell-notifications-panel"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors { top: true; bottom: true; left: true; right: true }
          color: "transparent"
          exclusionMode: ExclusionMode.Ignore

          Shortcut {
              sequences: ["Escape"]
              onActivated: root.hide()
          }

          // Outside click dismiss (excludes the bar area)
          MouseArea {
              anchors.fill: parent
              anchors.bottomMargin: 48
              onClicked: root.hide()
          }

          Rectangle {
              id: notifCard
              width: 560
              height: Math.min(720, panel.height - 80)
              anchors.bottom: parent.bottom
              anchors.right: parent.right
              anchors.bottomMargin: 52
              anchors.rightMargin: 8

              color: "${ca "base00" "e6"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"
              clip: true

              property real slideY: root.opened ? 0 : 18
              transform: Translate { y: notifCard.slideY }
              transformOrigin: Item.BottomRight
              opacity: root.opened ? 1 : 0
              scale: root.opened ? 1 : 0.96

              Behavior on opacity { NumberAnimation { duration: 220; easing.type: Easing.OutCubic } }
              Behavior on scale   { NumberAnimation { duration: 280; easing.type: Easing.OutQuint } }
              Behavior on slideY  { NumberAnimation { duration: 280; easing.type: Easing.OutQuint } }

              // Swallow clicks so the outside dismiss doesn't fire
              MouseArea {
                  anchors.fill: parent
                  onClicked: {}
              }

              ColumnLayout {
                  anchors.fill: parent
                  anchors.margins: 16
                  spacing: 12

                  // ── Header ──────────────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      spacing: 8

                      Text {
                          text: "󰂚 Notifications"
                          color: "${c "base0A"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 15
                          font.weight: Font.Medium
                      }

                      Rectangle {
                          Layout.preferredHeight: 20
                          Layout.preferredWidth: countText.implicitWidth + 14
                          radius: 10
                          color: "${ca "base02" "cc"}"
                          visible: root.histModel && root.histModel.count > 0
                          Text {
                              id: countText
                              anchors.centerIn: parent
                              text: root.histModel ? root.histModel.count : 0
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 10
                              font.weight: Font.Medium
                          }
                      }

                      Item { Layout.fillWidth: true }

                      // Clear all
                      Rectangle {
                          visible: root.histModel && root.histModel.count > 0
                          Layout.preferredHeight: 26
                          Layout.preferredWidth: clearText.implicitWidth + 18
                          radius: 6
                          color: clearHover.hovered
                              ? "${ca "base08" "33"}"
                              : "${ca "base01" "75"}"
                          border.width: 1
                          border.color: clearHover.hovered ? "${c "base08"}" : "${c "base02"}"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          Behavior on border.color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: clearHover }
                          Text {
                              id: clearText
                              anchors.centerIn: parent
                              text: "Clear all"
                              color: clearHover.hovered ? "${c "base08"}" : "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 10
                              font.weight: Font.Medium
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: if (root.notifications)
                                  root.notifications.clearHistory()
                          }
                      }

                      // Close
                      Text {
                          text: "󰅖"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 16
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: root.hide()
                          }
                      }
                  }

                  Rectangle {
                      Layout.fillWidth: true
                      height: 1
                      color: "${c "base02"}"
                  }

                  // ── Empty state ──────────────────────────────
                  Item {
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      visible: !root.histModel || root.histModel.count === 0

                      ColumnLayout {
                          anchors.centerIn: parent
                          spacing: 6
                          Text {
                              Layout.alignment: Qt.AlignHCenter
                              text: "󰂛"
                              color: "${c "base03"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 42
                          }
                          Text {
                              Layout.alignment: Qt.AlignHCenter
                              text: "No notifications"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 13
                          }
                          Text {
                              Layout.alignment: Qt.AlignHCenter
                              text: "You're all caught up."
                              color: "${c "base03"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 10
                          }
                      }
                  }

                  // ── Notification list ────────────────────────
                  ListView {
                      id: notifList
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      visible: root.histModel && root.histModel.count > 0
                      clip: true
                      spacing: 8
                      model: root.histModel
                      boundsBehavior: Flickable.StopAtBounds
                      ScrollBar.vertical: ScrollBar {
                          policy: ScrollBar.AsNeeded
                          contentItem: Rectangle {
                              implicitWidth: 4
                              radius: 2
                              color: "${ca "base04" "80"}"
                          }
                      }

                      delegate: Rectangle {
                          id: notifDelegate
                          required property int index
                          required property string summary
                          required property string body
                          required property string appName
                          required property string image
                          required property string appIcon
                          required property string time
                          required property int urgency

                          width: notifList.width
                          height: row.implicitHeight + 20
                          color: rowHover.hovered
                              ? "${ca "base01" "cc"}"
                              : "${ca "base00" "80"}"
                          radius: 10
                          border.width: 1
                          border.color: urgency === 2
                              ? "${c "base08"}"
                              : (rowHover.hovered ? "${c "base03"}" : "${c "base02"}")
                          Behavior on color       { ColorAnimation { duration: 100 } }
                          Behavior on border.color { ColorAnimation { duration: 100 } }

                          HoverHandler { id: rowHover }

                          RowLayout {
                              id: row
                              anchors.fill: parent
                              anchors.margins: 12
                              spacing: 12

                              // ── Avatar / icon ──────────────────
                              Item {
                                  id: notifAvatar
                                  Layout.preferredWidth: 44
                                  Layout.preferredHeight: 44
                                  Layout.alignment: Qt.AlignTop

                                  readonly property bool hasImage:
                                      notifDelegate.image !== ""
                                  readonly property string resolvedAppIcon:
                                      notifDelegate.appIcon !== ""
                                          ? Quickshell.iconPath(notifDelegate.appIcon, "")
                                          : ""
                                  readonly property bool hasAppIcon:
                                      resolvedAppIcon !== ""
                                  readonly property string fallbackLetter:
                                      notifDelegate.appName.length > 0
                                          ? notifDelegate.appName.charAt(0).toUpperCase()
                                          : "?"

                                  ClippingRectangle {
                                      anchors.fill: parent
                                      visible: notifAvatar.hasImage
                                      radius: width / 2
                                      color: "${ca "base01" "cc"}"
                                      Image {
                                          anchors.fill: parent
                                          source: notifDelegate.image
                                          sourceSize.width: 88
                                          sourceSize.height: 88
                                          fillMode: Image.PreserveAspectCrop
                                          smooth: true
                                          cache: false
                                      }
                                  }

                                  Image {
                                      anchors.fill: parent
                                      visible: !notifAvatar.hasImage
                                          && notifAvatar.hasAppIcon
                                      source: notifAvatar.hasAppIcon
                                          ? notifAvatar.resolvedAppIcon
                                          : ""
                                      sourceSize.width: 88
                                      sourceSize.height: 88
                                      fillMode: Image.PreserveAspectFit
                                      smooth: true
                                  }

                                  Rectangle {
                                      anchors.fill: parent
                                      visible: !notifAvatar.hasImage
                                          && !notifAvatar.hasAppIcon
                                      radius: width / 2
                                      color: "${ca "base0D" "55"}"
                                      border.width: 1
                                      border.color: "${ca "base0D" "aa"}"
                                      Text {
                                          anchors.centerIn: parent
                                          text: notifAvatar.fallbackLetter
                                          color: "${c "base05"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 20
                                          font.weight: Font.Bold
                                      }
                                  }

                                  Rectangle {
                                      visible: notifAvatar.hasImage
                                          && notifAvatar.hasAppIcon
                                      anchors.right: parent.right
                                      anchors.bottom: parent.bottom
                                      width: 20
                                      height: 20
                                      radius: 10
                                      color: "${ca "base00" "ee"}"
                                      border.width: 1
                                      border.color: "${c "base02"}"
                                      Image {
                                          anchors.fill: parent
                                          anchors.margins: 2
                                          source: notifAvatar.resolvedAppIcon
                                          sourceSize.width: 40
                                          sourceSize.height: 40
                                          fillMode: Image.PreserveAspectFit
                                          smooth: true
                                      }
                                  }
                              }

                              ColumnLayout {
                                  Layout.fillWidth: true
                                  spacing: 2

                                  RowLayout {
                                      Layout.fillWidth: true
                                      spacing: 6
                                      Text {
                                          text: notifDelegate.appName
                                          color: "${c "base0D"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 11
                                          font.weight: Font.Medium
                                          elide: Text.ElideRight
                                          Layout.maximumWidth: 220
                                      }
                                      Item { Layout.fillWidth: true }
                                      Text {
                                          text: notifDelegate.time
                                          color: "${c "base04"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 10
                                      }
                                  }
                                  Text {
                                      Layout.fillWidth: true
                                      text: notifDelegate.summary
                                      color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 14
                                      font.weight: Font.Medium
                                      wrapMode: Text.Wrap
                                      maximumLineCount: 2
                                      elide: Text.ElideRight
                                  }
                                  Text {
                                      Layout.fillWidth: true
                                      visible: notifDelegate.body !== ""
                                      text: notifDelegate.body
                                      color: "${c "base06"}"
                                      linkColor: "${c "base0C"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 12
                                      wrapMode: Text.Wrap
                                      textFormat: Text.RichText
                                      maximumLineCount: 8
                                      elide: Text.ElideRight
                                      onLinkActivated: function(link) {
                                          Qt.openUrlExternally(link)
                                      }
                                      HoverHandler {
                                          cursorShape: parent.hoveredLink !== ""
                                              ? Qt.PointingHandCursor
                                              : Qt.ArrowCursor
                                      }
                                  }
                              }

                              // Close
                              Text {
                                  text: "󰅖"
                                  color: closeHover.hovered ? "${c "base08"}" : "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 14
                                  Layout.alignment: Qt.AlignTop
                                  Behavior on color { ColorAnimation { duration: 100 } }
                                  HoverHandler { id: closeHover }
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: if (root.notifications)
                                          root.notifications.removeHistory(notifDelegate.index)
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
