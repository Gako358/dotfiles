{
  c,
  ca,
  lib,
  battery ? false,
}:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Widgets
  import Quickshell.Wayland
  import Quickshell.Io
  import Quickshell.Services.Mpris
  import Quickshell.Services.Pipewire
  ${lib.optionalString battery "import Quickshell.Services.UPower"}

  Scope {
      id: root

      property var notifications: null
      property bool opened: false

      signal monitorsRequested()
      signal wallpaperRequested()
      signal sessionRequested()
      signal audioRequested()
      signal networkRequested()
      signal batteryRequested()
      signal processesRequested(string sortMode)

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
      property string volPctStr: "—"
      property bool   volMuted:  false
      property string netState:  "off"
      property int    netSignal: 0
      property string netSsid:   ""

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
      Process {
          id: netProc
          command: ["sh", "-c",
              "act=$(nmcli -t -f TYPE,STATE,CONNECTION device status 2>/dev/null | awk -F: '$2==\"connected\"{print $1\":\"$3; exit}'); " +
              "if echo \"$act\" | grep -q '^wifi:'; then " +
              "  ssid=$(echo \"$act\" | cut -d: -f2-); " +
              "  sig=$(nmcli -t -f IN-USE,SIGNAL device wifi 2>/dev/null | awk -F: '$1==\"*\"{print $2; exit}'); " +
              "  echo \"wifi|$sig|$ssid\"; " +
              "elif echo \"$act\" | grep -q '^ethernet:'; then " +
              "  echo \"wired|100|$(echo \"$act\" | cut -d: -f2-)\"; " +
              "else echo \"off|0|\"; fi"]
          stdout: StdioCollector { id: netOut }
          onExited: {
              var parts = ((netOut.text || "").trim() || "off|0|").split("|")
              root.netState  = parts[0] || "off"
              root.netSignal = parseInt(parts[1] || "0") || 0
              root.netSsid   = parts[2] || ""
          }
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
              netProc.running = true
          }
      }

      // Volume tracking via Pipewire
      PwObjectTracker {
          objects: Pipewire.defaultAudioSink ? [Pipewire.defaultAudioSink] : []
      }
      readonly property var audioSink: Pipewire.defaultAudioSink
      Binding {
          target: root
          property: "volPctStr"
          value: root.audioSink && root.audioSink.audio
              ? Math.round(root.audioSink.audio.volume * 100) + "%"
              : "—"
      }
      Binding {
          target: root
          property: "volMuted"
          value: root.audioSink && root.audioSink.audio
              ? root.audioSink.audio.muted
              : false
      }

      function volIcon(p, m) {
          if (m || p <= 0) return "󰝟"
          if (p < 34)      return "󰕿"
          if (p < 67)      return "󰖀"
          return "󰕾"
      }
      function wifiIcon(s) {
          if (s >= 75) return "󰤨"
          if (s >= 50) return "󰤥"
          if (s >= 25) return "󰤢"
          if (s > 0)   return "󰤟"
          return "󰤮"
      }
      ${lib.optionalString battery ''
        readonly property var batDevice:  UPower.displayDevice
        readonly property bool hasBat:
            batDevice && batDevice.isPresent
            && batDevice.type === UPowerDeviceType.Battery
        readonly property real batPct:      hasBat ? batDevice.percentage * 100 : 0
        readonly property bool batCharging:
            hasBat && (batDevice.state === UPowerDeviceState.Charging
                    || batDevice.state === UPowerDeviceState.FullyCharged)

        function batteryIcon(p, charging) {
            if (charging) return "󰂄"
            if (p >= 95) return "󰁹"
            if (p >= 80) return "󰂂"
            if (p >= 65) return "󰂀"
            if (p >= 50) return "󰁾"
            if (p >= 35) return "󰁼"
            if (p >= 20) return "󰁻"
            if (p >= 10) return "󰁺"
            return "󰂎"
        }
        function batteryColor(p, charging) {
            if (charging) return "${c "base0B"}"
            if (p <= 15)  return "${c "base08"}"
            if (p <= 30)  return "${c "base0A"}"
            return "${c "base0B"}"
        }
      ''}

      PanelWindow {
          id: panel
          visible: root.opened || dashCard.opacity > 0.01

          WlrLayershell.namespace: "quickshell-dashboard"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          exclusionMode: ExclusionMode.Ignore

              // Win11 Action Center: slides up from bottom-right corner
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
                  id: dashCard
                  width: 420
                  height: 680
                  anchors.bottom: parent.bottom
                  anchors.right: parent.right
                  anchors.bottomMargin: 52
                  anchors.rightMargin: 8
              color: "${ca "base00" "e6"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"

              // Slide up from bottom on open
              property real slideY: root.opened ? 0 : 18
              transform: Translate { y: dashCard.slideY }
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

              clip: true

              ScrollView {
                  anchors.fill: parent
                  anchors.margins: 4
                  contentWidth: availableWidth
                  topPadding: 14
                  bottomPadding: 14
                  leftPadding: 14
                  rightPadding: 14
                  ScrollBar.horizontal.policy: ScrollBar.AlwaysOff
                  ScrollBar.vertical.policy: ScrollBar.AsNeeded

              ColumnLayout {
                  width: parent.width
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

                  // ── Quick actions (3 buttons row) ────────────
                  GridLayout {
                      Layout.fillWidth: true
                      columns: 3
                      rowSpacing: 8
                      columnSpacing: 8

                      // Displays button
                      Rectangle {
                          id: displaysBtn
                          Layout.fillWidth: true
                          Layout.preferredHeight: 52
                          color: displaysHover.hovered
                              ? "${ca "base02" "cc"}"
                              : "${ca "base01" "75"}"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          radius: 12
                          border.width: 1
                          border.color: "${c "base02"}"
                          HoverHandler { id: displaysHover }
                          ColumnLayout {
                              anchors.centerIn: parent
                              spacing: 2
                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  text: "󰍹"
                                  color: "${c "base0D"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 18
                              }
                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  text: "Displays"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: { root.hide(); root.monitorsRequested() }
                          }
                      }

                      // Wallpaper button
                      Rectangle {
                          id: wallpaperBtn
                          Layout.fillWidth: true
                          Layout.preferredHeight: 52
                          color: wallpaperHover.hovered
                              ? "${ca "base02" "cc"}"
                              : "${ca "base01" "75"}"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          radius: 12
                          border.width: 1
                          border.color: "${c "base02"}"
                          HoverHandler { id: wallpaperHover }
                          ColumnLayout {
                              anchors.centerIn: parent
                              spacing: 2
                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  text: "󰸉"
                                  color: "${c "base0E"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 18
                              }
                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  text: "Wallpaper"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: { root.hide(); root.wallpaperRequested() }
                          }
                      }

                      // Session / reboot button
                      Rectangle {
                          id: sessionBtn
                          Layout.fillWidth: true
                          Layout.preferredHeight: 52
                          color: sessionBtnHover.hovered
                              ? "${ca "base08" "33"}"
                              : "${ca "base01" "75"}"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          radius: 12
                          border.width: 1
                          border.color: sessionBtnHover.hovered ? "${c "base08"}" : "${c "base02"}"
                          Behavior on border.color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: sessionBtnHover }
                          ColumnLayout {
                              anchors.centerIn: parent
                              spacing: 2
                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  text: "󰐥"
                                  color: "${c "base08"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 18
                              }
                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  text: "Session"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: { root.hide(); root.sessionRequested() }
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
                      Layout.preferredHeight: ${if battery then "240" else "210"}
                      color: "${ca "base01" "75"}"
                      radius: 12
                      border.width: 1
                      border.color: "${c "base02"}"

                      ColumnLayout {
                          anchors.fill: parent
                          anchors.margins: 14
                          spacing: 8

                          // CPU — click to open process panel sorted by CPU
                          Rectangle {
                              Layout.fillWidth: true
                              height: 28
                              radius: 6
                              color: cpuRowHover.hovered
                                  ? "${ca "base02" "cc"}"
                                  : "transparent"
                              Behavior on color { ColorAnimation { duration: 100 } }
                              HoverHandler { id: cpuRowHover }

                              RowLayout {
                                  anchors.fill: parent
                                  anchors.leftMargin: 4
                                  anchors.rightMargin: 4
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

                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: {
                                      root.hide()
                                      root.processesRequested("cpu")
                                  }
                              }
                          }

                          // Memory — click to open process panel sorted by MEM
                          Rectangle {
                              Layout.fillWidth: true
                              height: 28
                              radius: 6
                              color: memRowHover.hovered
                                  ? "${ca "base02" "cc"}"
                                  : "transparent"
                              Behavior on color { ColorAnimation { duration: 100 } }
                              HoverHandler { id: memRowHover }

                              RowLayout {
                                  anchors.fill: parent
                                  anchors.leftMargin: 4
                                  anchors.rightMargin: 4
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

                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: {
                                      root.hide()
                                      root.processesRequested("mem")
                                  }
                              }
                          }
                          ${lib.optionalString battery ''
                            // Battery — click to open battery panel
                            Rectangle {
                                Layout.fillWidth: true
                                height: 28
                                radius: 6
                                visible: root.hasBat
                                color: batRowHover.hovered
                                    ? "${ca "base02" "cc"}"
                                    : "transparent"
                                Behavior on color { ColorAnimation { duration: 100 } }
                                HoverHandler { id: batRowHover }

                                RowLayout {
                                    anchors.fill: parent
                                    anchors.leftMargin: 4
                                    anchors.rightMargin: 4
                                    Text {
                                        text: root.batteryIcon(root.batPct, root.batCharging) + " Battery"
                                        color: root.batteryColor(root.batPct, root.batCharging)
                                        font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                    }
                                    Item { Layout.fillWidth: true }
                                    Text {
                                        text: Math.round(root.batPct) + "%"
                                            + (root.batCharging ? " (charging)" : "")
                                        color: "${c "base05"}"
                                        font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                    }
                                }

                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: {
                                        root.hide()
                                        root.batteryRequested()
                                    }
                                }
                            }
                          ''}
                          // Volume — click to open volume panel, scroll to adjust
                          Rectangle {
                              Layout.fillWidth: true
                              height: 28
                              radius: 6
                              color: volRowHover.hovered
                                  ? "${ca "base02" "cc"}"
                                  : "transparent"
                              Behavior on color { ColorAnimation { duration: 100 } }
                              HoverHandler { id: volRowHover }

                              RowLayout {
                                  anchors.fill: parent
                                  anchors.leftMargin: 4
                                  anchors.rightMargin: 4
                                  Text {
                                      text: root.volIcon(
                                          root.audioSink && root.audioSink.audio
                                              ? root.audioSink.audio.volume * 100 : 0,
                                          root.volMuted) + " Volume"
                                      color: root.volMuted ? "${c "base08"}" : "${c "base0E"}"
                                      font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                  }
                                  Item { Layout.fillWidth: true }
                                  Text {
                                      text: root.volMuted ? "muted" : root.volPctStr
                                      color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                  }
                              }

                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  acceptedButtons: Qt.LeftButton | Qt.MiddleButton
                                  onClicked: function(mouse) {
                                      if (mouse.button === Qt.MiddleButton) {
                                          if (root.audioSink && root.audioSink.audio)
                                              root.audioSink.audio.muted = !root.audioSink.audio.muted
                                      } else {
                                          root.hide()
                                          root.audioRequested()
                                      }
                                  }
                                  onWheel: function(wheel) {
                                      if (!root.audioSink || !root.audioSink.audio) return
                                      var step = wheel.angleDelta.y > 0 ? 0.03 : -0.03
                                      var v = Math.max(0, Math.min(1, root.audioSink.audio.volume + step))
                                      root.audioSink.audio.volume = v
                                  }
                              }
                          }

                          // Network — click to open network panel
                          Rectangle {
                              Layout.fillWidth: true
                              height: 28
                              radius: 6
                              color: netRowHover.hovered
                                  ? "${ca "base02" "cc"}"
                                  : "transparent"
                              Behavior on color { ColorAnimation { duration: 100 } }
                              HoverHandler { id: netRowHover }

                              RowLayout {
                                  anchors.fill: parent
                                  anchors.leftMargin: 4
                                  anchors.rightMargin: 4
                                  Text {
                                      text: (root.netState === "wired" ? "󰈀" : root.wifiIcon(root.netSignal)) + " Network"
                                      color: root.netState === "off" ? "${c "base08"}" : "${c "base0B"}"
                                      font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                  }
                                  Item { Layout.fillWidth: true }
                                  Text {
                                      text: root.netState === "off"
                                          ? "offline"
                                          : (root.netState === "wired"
                                              ? "Wired"
                                              : (root.netSsid.length > 16
                                                  ? root.netSsid.substring(0, 16) + "…"
                                                  : root.netSsid))
                                      color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                      elide: Text.ElideRight
                                  }
                              }

                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: {
                                      root.hide()
                                      root.networkRequested()
                                  }
                              }
                          }
                          // Uptime
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
                                  id: histDelegate
                                  required property int index
                                  required property string summary
                                  required property string body
                                  required property string appName
                                  required property string image
                                  required property string appIcon
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

                                      // ── Avatar / icon (compact) ──
                                      Item {
                                          id: histAvatar
                                          Layout.preferredWidth: 28
                                          Layout.preferredHeight: 28
                                          Layout.alignment: Qt.AlignTop

                                          readonly property bool hasImage:
                                              histDelegate.image !== ""
                                          readonly property string resolvedAppIcon:
                                              histDelegate.appIcon !== ""
                                                  ? Quickshell.iconPath(histDelegate.appIcon, "")
                                                  : ""
                                          readonly property bool hasAppIcon:
                                              resolvedAppIcon !== ""
                                          readonly property string fallbackLetter:
                                              histDelegate.appName.length > 0
                                                  ? histDelegate.appName.charAt(0).toUpperCase()
                                                  : "?"

                                          ClippingRectangle {
                                              anchors.fill: parent
                                              visible: histAvatar.hasImage
                                              radius: width / 2
                                              color: "${ca "base01" "cc"}"
                                              Image {
                                                  anchors.fill: parent
                                                  source: histDelegate.image
                                                  sourceSize.width: 56
                                                  sourceSize.height: 56
                                                  fillMode: Image.PreserveAspectCrop
                                                  smooth: true
                                                  cache: false
                                              }
                                          }

                                          Image {
                                              anchors.fill: parent
                                              visible: !histAvatar.hasImage
                                                  && histAvatar.hasAppIcon
                                              source: histAvatar.hasAppIcon
                                                  ? histAvatar.resolvedAppIcon
                                                  : ""
                                              sourceSize.width: 56
                                              sourceSize.height: 56
                                              fillMode: Image.PreserveAspectFit
                                              smooth: true
                                          }

                                          Rectangle {
                                              anchors.fill: parent
                                              visible: !histAvatar.hasImage
                                                  && !histAvatar.hasAppIcon
                                              radius: width / 2
                                              color: "${ca "base0D" "55"}"
                                              border.width: 1
                                              border.color: "${ca "base0D" "aa"}"
                                              Text {
                                                  anchors.centerIn: parent
                                                  text: histAvatar.fallbackLetter
                                                  color: "${c "base05"}"
                                                  font.family: "RobotoMono Nerd Font"
                                                  font.pixelSize: 12
                                                  font.weight: Font.Bold
                                              }
                                          }

                                          Rectangle {
                                              visible: histAvatar.hasImage
                                                  && histAvatar.hasAppIcon
                                              anchors.right: parent.right
                                              anchors.bottom: parent.bottom
                                              width: 13
                                              height: 13
                                              radius: 6.5
                                              color: "${ca "base00" "ee"}"
                                              border.width: 1
                                              border.color: "${c "base02"}"
                                              Image {
                                                  anchors.fill: parent
                                                  anchors.margins: 1
                                                  source: histAvatar.resolvedAppIcon
                                                  sourceSize.width: 24
                                                  sourceSize.height: 24
                                                  fillMode: Image.PreserveAspectFit
                                                  smooth: true
                                              }
                                          }
                                      }

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
              } // ScrollView
          }
      }
  }
''
