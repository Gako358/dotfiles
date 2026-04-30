{ c, ca }:
''
  import QtQuick
  import QtQuick.Layouts
  import Quickshell
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
              top: true
              right: true
          }
          margins {
              top: 50
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

                              Image {
                                  visible: toast.image !== ""
                                  source: toast.image
                                  sourceSize.width: 36
                                  sourceSize.height: 36
                                  Layout.preferredWidth: 36
                                  Layout.preferredHeight: 36
                                  fillMode: Image.PreserveAspectFit
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
