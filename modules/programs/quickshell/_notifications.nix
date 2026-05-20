{ c, ca }:
''
  import QtQuick
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Widgets
  import Quickshell.Wayland
  import Quickshell.Services.Notifications

  Scope {
      id: root

      ListModel { id: toasts }
      ListModel { id: history }
      readonly property int maxHistory: 50

      property alias historyModel: history

      function clearHistory() {
          history.clear()
      }
      function removeHistory(idx) {
          if (idx >= 0 && idx < history.count) history.remove(idx)
      }

      NotificationServer {
          id: server
          keepOnReload: false
          actionsSupported: true
          bodyMarkupSupported: true
          bodySupported: true
          imageSupported: true

          onNotification: function(notif) {
              notif.tracked = true
              var entry = {
                  nid: notif.id,
                  summary: notif.summary || "",
                  body: notif.body || "",
                  appName: notif.appName || "",
                  image: notif.image || "",
                  appIcon: notif.appIcon || "",
                  urgency: notif.urgency,
                  time: Qt.formatDateTime(new Date(), "hh:mm")
              }
              toasts.append(entry)
              history.insert(0, entry)
              while (history.count > root.maxHistory)
                  history.remove(history.count - 1)
          }
      }

      PanelWindow {
          id: stack
          visible: toasts.count > 0

          WlrLayershell.namespace: "quickshell-notifications"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.None

          anchors {
              bottom: true
              right: true
          }
          margins {
              bottom: 56
              right: 8
          }

          implicitWidth: 420
          implicitHeight: stackCol.implicitHeight + 8
          color: "transparent"

          ColumnLayout {
              id: stackCol
              anchors.fill: parent
              anchors.margins: 4
              spacing: 10

              Repeater {
                  model: toasts

                  Rectangle {
                      id: toast
                      required property int index
                      required property int nid
                      required property string summary
                      required property string body
                      required property string appName
                      required property string image
                      required property string appIcon
                      required property int urgency

                      Layout.fillWidth: true
                      Layout.preferredHeight: toastCol.implicitHeight + 24
                      color: "${ca "base00" "ee"}"
                      radius: 12
                      border.width: 1
                      border.color: toast.urgency === NotificationUrgency.Critical
                          ? "${c "base08"}"
                          : "${c "base02"}"

                      opacity: 0
                      Component.onCompleted: opacity = 1
                      Behavior on opacity { NumberAnimation { duration: 220 } }

                      Timer {
                          running: toast.urgency !== NotificationUrgency.Critical
                          interval: 6000
                          onTriggered: toasts.remove(toast.index)
                      }

                      ColumnLayout {
                          id: toastCol
                          anchors.fill: parent
                          anchors.margins: 12
                          spacing: 4

                          RowLayout {
                              Layout.fillWidth: true
                              spacing: 10

                              // ── Avatar / icon block ──────────────
                              Item {
                                  id: avatar
                                  Layout.preferredWidth: 40
                                  Layout.preferredHeight: 40
                                  Layout.alignment: Qt.AlignTop

                                  readonly property bool hasImage:
                                      toast.image !== ""
                                  readonly property string resolvedAppIcon:
                                      toast.appIcon !== ""
                                          ? Quickshell.iconPath(toast.appIcon, "")
                                          : ""
                                  readonly property bool hasAppIcon:
                                      resolvedAppIcon !== ""
                                  readonly property string fallbackLetter:
                                      toast.appName.length > 0
                                          ? toast.appName.charAt(0).toUpperCase()
                                          : "?"

                                  // (1) Circular user avatar.
                                  ClippingRectangle {
                                      anchors.fill: parent
                                      visible: avatar.hasImage
                                      radius: width / 2
                                      color: "${ca "base01" "cc"}"

                                      Image {
                                          anchors.fill: parent
                                          source: toast.image
                                          sourceSize.width: 80
                                          sourceSize.height: 80
                                          fillMode: Image.PreserveAspectCrop
                                          smooth: true
                                          cache: false
                                      }
                                  }

                                  // (2) Square app icon (no user image).
                                  Image {
                                      anchors.fill: parent
                                      visible: !avatar.hasImage
                                          && avatar.hasAppIcon
                                      source: avatar.hasAppIcon
                                          ? avatar.resolvedAppIcon
                                          : ""
                                      sourceSize.width: 80
                                      sourceSize.height: 80
                                      fillMode: Image.PreserveAspectFit
                                      smooth: true
                                  }

                                  // (3) Letter fallback.
                                  Rectangle {
                                      anchors.fill: parent
                                      visible: !avatar.hasImage
                                          && !avatar.hasAppIcon
                                      radius: width / 2
                                      color: "${ca "base0D" "55"}"
                                      border.width: 1
                                      border.color: "${ca "base0D" "aa"}"
                                      Text {
                                          anchors.centerIn: parent
                                          text: avatar.fallbackLetter
                                          color: "${c "base05"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 18
                                          font.weight: Font.Bold
                                      }
                                  }

                                  // App-icon badge (corner) — only when
                                  // we already show a user avatar and
                                  // also have a distinct app icon.
                                  Rectangle {
                                      visible: avatar.hasImage
                                          && avatar.hasAppIcon
                                      anchors.right: parent.right
                                      anchors.bottom: parent.bottom
                                      width: 18
                                      height: 18
                                      radius: 9
                                      color: "${ca "base00" "ee"}"
                                      border.width: 1
                                      border.color: "${c "base02"}"

                                      Image {
                                          anchors.fill: parent
                                          anchors.margins: 2
                                          source: avatar.resolvedAppIcon
                                          sourceSize.width: 32
                                          sourceSize.height: 32
                                          fillMode: Image.PreserveAspectFit
                                          smooth: true
                                      }
                                  }
                              }

                              ColumnLayout {
                                  Layout.fillWidth: true
                                  spacing: 1
                                  Text {
                                      Layout.fillWidth: true
                                      text: toast.appName
                                      color: "${c "base0D"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 11
                                      font.weight: Font.Medium
                                      elide: Text.ElideRight
                                  }
                                  Text {
                                      Layout.fillWidth: true
                                      text: toast.summary
                                      color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 15
                                      font.weight: Font.Medium
                                      elide: Text.ElideRight
                                  }
                              }

                              Text {
                                  text: "󰅖"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 16
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: toasts.remove(toast.index)
                                  }
                              }
                          }

                          Text {
                              Layout.fillWidth: true
                              visible: toast.body !== ""
                              text: toast.body
                              color: "${c "base06"}"
                              linkColor: "${c "base0C"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 13
                              wrapMode: Text.Wrap
                              textFormat: Text.RichText
                              maximumLineCount: 6
                              elide: Text.ElideRight
                              onLinkActivated: function(link) { Qt.openUrlExternally(link) }
                              HoverHandler {
                                  cursorShape: parent.hoveredLink !== ""
                                      ? Qt.PointingHandCursor
                                      : Qt.ArrowCursor
                              }
                          }
                      }

                      MouseArea {
                          anchors.fill: parent
                          acceptedButtons: Qt.RightButton
                          onClicked: toasts.remove(toast.index)
                          propagateComposedEvents: true
                      }
                  }
              }
          }
      }
  }
''
