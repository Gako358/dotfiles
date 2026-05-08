{ c, ca }:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io
  import Quickshell.Services.Mpris

  Scope {
      id: root

      property var notifications: null
      property bool opened: false

      function toggle() { root.opened = !root.opened }
      function show()   { root.opened = true }
      function hide()   { root.opened = false }

      IpcHandler {
          target: "dashboard"
          function toggle() { root.toggle() }
          function show()   { root.show() }
          function hide()   { root.hide() }
      }

      property string cpuPct:  "—"
      property string memUsed: "—"
      property string uptime:  "—"

      Process {
          id: cpuProc
          command: ["sh", "-c",
              "top -bn1 | awk '/Cpu/ { printf \"%.0f\", 100 - $8 }'"]
          stdout: StdioCollector { id: cpuOut }
          onExited: root.cpuPct = (cpuOut.text || "").trim() + "%"
      }
      Process {
          id: memProc
          command: ["sh", "-c",
              "free -h --si | awk '/Mem:/ { print $3 \" / \" $2 }'"]
          stdout: StdioCollector { id: memOut }
          onExited: root.memUsed = (memOut.text || "").trim()
      }
      Process {
          id: upProc
          command: ["sh", "-c",
              "awk '{u=int($1); d=int(u/86400); h=int((u%86400)/3600); m=int((u%3600)/60); s=\"\"; if(d>0) s=s d\"d \"; if(h>0) s=s h\"h \"; s=s m\"m\"; print s}' /proc/uptime"]
          stdout: StdioCollector { id: upOut }
          onExited: root.uptime = (upOut.text || "").trim() || "—"
      }
      Timer {
          id: pollTimer
          running: root.opened
          repeat: true
          interval: 2000
          triggeredOnStart: true
          onTriggered: {
              cpuProc.running = true
              memProc.running = true
              upProc.running = true
          }
      }

      PanelWindow {
          id: panel
          visible: root.opened || dashCard.opacity > 0.01

          WlrLayershell.namespace: "quickshell-dashboard"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          exclusionMode: ExclusionMode.Ignore

          anchors {
              top: true
              right: true
              bottom: true
          }
          margins {
              top: 50
              right: 8
              bottom: 8
          }
          implicitWidth: 420
          color: "transparent"

          Shortcut {
              sequences: ["Escape"]
              onActivated: root.hide()
          }

          Rectangle {
              id: dashCard
              anchors.fill: parent
              color: "${ca "base00" "e6"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"

              property real slideY: root.opened ? 0 : -18
              transform: Translate { y: dashCard.slideY }
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
                  anchors.fill: parent
                  anchors.margins: 18
                  spacing: 16

                  // ── Greeting / Time ──────────────────────────
                  ColumnLayout {
                      spacing: 2
                      Text {
                          id: bigTime
                          text: ""
                          color: "${c "base05"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 38
                          font.weight: Font.Light
                          Timer {
                              running: true; repeat: true; interval: 1000; triggeredOnStart: true
                              onTriggered: bigTime.text =
                                  Qt.formatDateTime(new Date(), "hh:mm")
                          }
                      }
                      Text {
                          id: bigDate
                          text: ""
                          color: "${c "base0D"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 13
                          Timer {
                              running: true; repeat: true; interval: 30000; triggeredOnStart: true
                              onTriggered: bigDate.text =
                                  Qt.formatDateTime(new Date(), "dddd, MMMM d")
                          }
                      }
                  }

                  // ── Media player (MPRIS) ─────────────────────
                  Rectangle {
                      id: mediaCard
                      Layout.fillWidth: true
                      Layout.preferredHeight: 120
                      color: "${ca "base01" "75"}"
                      radius: 12
                      border.width: 1
                      border.color: "${c "base02"}"

                      property var player: Mpris.players && Mpris.players.values.length > 0
                          ? Mpris.players.values[0]
                          : null

                      // trackArtists is a Qt list, not a JS array — manual join.
                      function artistsText() {
                          if (!mediaCard.player) return ""
                          var arts = mediaCard.player.trackArtists
                          if (!arts) return ""
                          var out = []
                          for (var i = 0; i < arts.length; ++i) out.push(arts[i])
                          return out.join(", ")
                      }

                      ColumnLayout {
                          anchors.fill: parent
                          anchors.margins: 12
                          spacing: 4

                          Text {
                              Layout.fillWidth: true
                              text: mediaCard.player
                                  ? (mediaCard.player.trackTitle || "—")
                                  : "Nothing playing"
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 14
                              elide: Text.ElideRight
                          }
                          Text {
                              Layout.fillWidth: true
                              text: mediaCard.artistsText()
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                              elide: Text.ElideRight
                          }

                          Item { Layout.fillHeight: true }

                          RowLayout {
                              Layout.alignment: Qt.AlignHCenter
                              spacing: 24

                              Text {
                                  text: "󰒮"
                                  color: "${c "base0D"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 20
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: if (mediaCard.player) mediaCard.player.previous()
                                  }
                              }
                              Text {
                                  text: mediaCard.player
                                      && mediaCard.player.playbackState === MprisPlaybackState.Playing
                                      ? "󰏤"
                                      : "󰐊"
                                  color: "${c "base0D"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 24
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: if (mediaCard.player) mediaCard.player.togglePlaying()
                                  }
                              }
                              Text {
                                  text: "󰒭"
                                  color: "${c "base0D"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 20
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: if (mediaCard.player) mediaCard.player.next()
                                  }
                              }
                          }
                      }
                  }

                  // ── System info card ─────────────────────────
                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 110
                      color: "${ca "base01" "75"}"
                      radius: 12
                      border.width: 1
                      border.color: "${c "base02"}"

                      ColumnLayout {
                          anchors.fill: parent
                          anchors.margins: 14
                          spacing: 8

                          RowLayout {
                              Layout.fillWidth: true
                              Text {
                                  text: "󰻠 CPU"; color: "${c "base0C"}"
                                  font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                              }
                              Item { Layout.fillWidth: true }
                              Text {
                                  text: root.cpuPct
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                              }
                          }
                          RowLayout {
                              Layout.fillWidth: true
                              Text {
                                  text: "󰍛 Memory"; color: "${c "base0E"}"
                                  font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                              }
                              Item { Layout.fillWidth: true }
                              Text {
                                  text: root.memUsed
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                              }
                          }
                          RowLayout {
                              Layout.fillWidth: true
                              Text {
                                  text: "󰅐 Uptime"; color: "${c "base0A"}"
                                  font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                              }
                              Item { Layout.fillWidth: true }
                              Text {
                                  text: root.uptime
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                  elide: Text.ElideRight
                                  horizontalAlignment: Text.AlignRight
                              }
                          }
                      }
                  }

                  // ── Recent notifications ─────────────────────
                  Rectangle {
                      id: notifCard
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      Layout.minimumHeight: 200
                      color: "${ca "base01" "75"}"
                      radius: 12
                      border.width: 1
                      border.color: "${c "base02"}"

                      readonly property var histModel:
                          root.notifications ? root.notifications.historyModel : null

                      ColumnLayout {
                          anchors.fill: parent
                          anchors.margins: 12
                          spacing: 6

                          RowLayout {
                              Layout.fillWidth: true
                              Text {
                                  text: "󰂚 Notifications"
                                  color: "${c "base0A"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 13
                                  font.weight: Font.Medium
                              }
                              Item { Layout.fillWidth: true }
                              Text {
                                  visible: notifCard.histModel
                                      && notifCard.histModel.count > 0
                                  text: "Clear all"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: if (root.notifications)
                                          root.notifications.clearHistory()
                                  }
                              }
                          }

                          Text {
                              Layout.fillWidth: true
                              Layout.fillHeight: true
                              visible: !notifCard.histModel
                                  || notifCard.histModel.count === 0
                              horizontalAlignment: Text.AlignHCenter
                              verticalAlignment: Text.AlignVCenter
                              text: "No new notifications"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                          }

                          ListView {
                              id: notifList
                              Layout.fillWidth: true
                              Layout.fillHeight: true
                              visible: notifCard.histModel
                                  && notifCard.histModel.count > 0
                              clip: true
                              spacing: 6
                              model: notifCard.histModel
                              boundsBehavior: Flickable.StopAtBounds
                              ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

                              delegate: Rectangle {
                                  required property int index
                                  required property string summary
                                  required property string body
                                  required property string appName
                                  required property string time
                                  required property int urgency

                                  width: notifList.width
                                  height: row.implicitHeight + 12
                                  color: "${ca "base00" "80"}"
                                  radius: 8
                                  border.width: 1
                                  border.color: urgency === 2
                                      ? "${c "base08"}"
                                      : "${c "base02"}"

                                  RowLayout {
                                      id: row
                                      anchors.fill: parent
                                      anchors.margins: 8
                                      spacing: 8

                                      ColumnLayout {
                                          Layout.fillWidth: true
                                          spacing: 1
                                          RowLayout {
                                              Layout.fillWidth: true
                                              spacing: 6
                                              Text {
                                                  text: appName
                                                  color: "${c "base0D"}"
                                                  font.family: "RobotoMono Nerd Font"
                                                  font.pixelSize: 10
                                                  font.weight: Font.Medium
                                                  elide: Text.ElideRight
                                                  Layout.maximumWidth: 140
                                              }
                                              Item { Layout.fillWidth: true }
                                              Text {
                                                  text: time
                                                  color: "${c "base04"}"
                                                  font.family: "RobotoMono Nerd Font"
                                                  font.pixelSize: 10
                                              }
                                          }
                                          Text {
                                              Layout.fillWidth: true
                                              text: summary
                                              color: "${c "base05"}"
                                              font.family: "RobotoMono Nerd Font"
                                              font.pixelSize: 12
                                              elide: Text.ElideRight
                                          }
                                          Text {
                                              Layout.fillWidth: true
                                              visible: body !== ""
                                              text: body
                                              color: "${c "base06"}"
                                              linkColor: "${c "base0C"}"
                                              font.family: "RobotoMono Nerd Font"
                                              font.pixelSize: 11
                                              wrapMode: Text.Wrap
                                              textFormat: Text.RichText
                                              maximumLineCount: 2
                                              elide: Text.ElideRight
                                              onLinkActivated: function(link) {
                                                  Qt.openUrlExternally(link)
                                              }
                                          }
                                      }

                                      Text {
                                          text: "󰅖"
                                          color: "${c "base04"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 12
                                          Layout.alignment: Qt.AlignTop
                                          MouseArea {
                                              anchors.fill: parent
                                              cursorShape: Qt.PointingHandCursor
                                              onClicked: if (root.notifications)
                                                  root.notifications.removeHistory(index)
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }

                  // ── Calendar (simple month grid) ─────────────
                  Rectangle {
                      id: calCard
                      Layout.fillWidth: true
                      Layout.preferredHeight: 170
                      color: "${ca "base01" "75"}"
                      radius: 12
                      border.width: 1
                      border.color: "${c "base02"}"

                      property date today: new Date()
                      property int year:  today.getFullYear()
                      property int month: today.getMonth()

                      ColumnLayout {
                          anchors.fill: parent
                          anchors.margins: 12
                          spacing: 6

                          Text {
                              Layout.alignment: Qt.AlignHCenter
                              text: Qt.formatDateTime(calCard.today, "MMMM yyyy")
                              color: "${c "base0D"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 13
                          }

                          GridLayout {
                              Layout.fillWidth: true
                              Layout.fillHeight: true
                              columns: 7
                              rowSpacing: 2
                              columnSpacing: 2

                              Repeater {
                                  model: ["M","T","W","T","F","S","S"]
                                  Text {
                                      Layout.fillWidth: true
                                      horizontalAlignment: Text.AlignHCenter
                                      text: modelData
                                      color: "${c "base04"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 10
                                  }
                              }

                              Repeater {
                                  model: 42
                                  Item {
                                      id: cell
                                      Layout.fillWidth: true
                                      Layout.preferredHeight: 18
                                      property var firstOfMonth: new Date(calCard.year, calCard.month, 1)
                                      property int firstDow: (firstOfMonth.getDay() + 6) % 7
                                      property int dayNum: index - firstDow + 1
                                      property var cellDate: new Date(calCard.year, calCard.month, dayNum)
                                      property bool inMonth: dayNum >= 1
                                          && cellDate.getMonth() === calCard.month
                                      property bool isToday: inMonth
                                          && cellDate.getDate() === calCard.today.getDate()

                                      Rectangle {
                                          anchors.centerIn: parent
                                          width: 22; height: 18
                                          radius: 4
                                          color: cell.isToday ? "${c "base0D"}" : "transparent"
                                          Text {
                                              anchors.centerIn: parent
                                              text: cell.inMonth ? cell.dayNum : ""
                                              color: cell.isToday
                                                  ? "${c "base00"}"
                                                  : "${c "base05"}"
                                              font.family: "RobotoMono Nerd Font"
                                              font.pixelSize: 10
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
  }
''
