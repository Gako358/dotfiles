{
  c,
  ca,
  lib,
  lockMonitors ? [ ],
}:
let
  monitorsJs =
    if lockMonitors == [ ] then
      "[]"
    else
      "[" + (lib.concatMapStringsSep ", " (m: ''"${m}"'') lockMonitors) + "]";
in
''
  import QtQuick
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io
  import Quickshell.Services.Pam

  Scope {
      id: root

      // ── Lock state (shared across all surfaces) ──────
      property string password: ""
      property bool   authFailed: false
      property bool   authenticating: false

      signal shakeRequested()

      // ── Monitor selection ────────────────────────────
      property var lockMonitors: ${monitorsJs}

      readonly property string primaryScreenName: {
          var ms = root.lockMonitors
          if (!ms || ms.length === 0) return ""
          var screens = Quickshell.screens
          for (var i = 0; i < ms.length; ++i) {
              var ident = ms[i]
              if (!ident) continue
              // allow Hyprland-style "desc:..." identifiers
              if (ident.indexOf("desc:") === 0) ident = ident.substring(5)
              for (var j = 0; j < screens.length; ++j) {
                  var s = screens[j]
                  var desc = (s.name || "") + " "
                           + (s.model || "") + " "
                           + (s.serialNumber || "")
                  if (desc.indexOf(ident) >= 0) return s.name || ""
              }
          }
          return ""
      }

      function lock()   { sessionLock.locked = true }
      function unlock() { sessionLock.locked = false }
      function tryAuth() {
          if (root.authenticating) return
          if (root.password === "") return
          root.authFailed = false
          root.authenticating = true
          pam.start()
      }

      IpcHandler {
          target: "lock"
          function lock()   { root.lock() }
          function unlock() { root.unlock() }
      }

      PamContext {
          id: pam
          config: "quickshell"

          onPamMessage: function(msg) {
              respond(root.password)
          }
          onCompleted: function(result) {
              root.authenticating = false
              if (result === PamResult.Success) {
                  root.password = ""
                  root.authFailed = false
                  root.unlock()
              } else {
                  root.authFailed = true
                  root.password = ""
                  root.shakeRequested()
              }
          }
      }

      WlSessionLock {
          id: sessionLock
          locked: false

          WlSessionLockSurface {
              id: surface
              color: "${c "base00"}"

              readonly property bool isPrimary: {
                  if (root.lockMonitors.length === 0) return true
                  var primary = root.primaryScreenName
                  if (primary === "") return true
                  var s = surface.screen
                  if (!s) return false
                  return (s.name || "") === primary
              }

              // ── Blank surface for non-primary monitors ────
              Rectangle {
                  anchors.fill: parent
                  visible: !surface.isPrimary
                  color: "${c "base00"}"

                  TextInput {
                      id: blankCatcher
                      width: 1; height: 1
                      opacity: 0
                      focus: true
                      text: root.password
                      echoMode: TextInput.Password
                      enabled: !root.authenticating
                      onTextEdited: root.password = text
                      Keys.onEnterPressed: root.tryAuth()
                      Keys.onReturnPressed: root.tryAuth()
                  }
              }

              // ── Full Win7 desktop on the primary monitor ──
              Loader {
                  anchors.fill: parent
                  active: surface.isPrimary
                  sourceComponent: lockUiComponent
              }

              Component {
                  id: lockUiComponent

                  Rectangle {
                      id: desktop
                      anchors.fill: parent

                      // ── Aero-ish gradient "wallpaper" ────────
                      gradient: Gradient {
                          GradientStop { position: 0.0; color: "${c "base01"}" }
                          GradientStop { position: 0.5; color: "${c "base00"}" }
                          GradientStop { position: 1.0; color: "${c "base01"}" }
                      }

                      // React to PAM failure
                      Connections {
                          target: root
                          function onShakeRequested() { shakeAnim.start() }
                      }

                      // ╔════════════════════════════════════════╗
                      // ║  CALENDAR APP WINDOW (top-left)        ║
                      // ╚════════════════════════════════════════╝
                      Rectangle {
                          id: calWindow
                          x: 56
                          y: 56
                          width: Math.min(parent.width * 0.42, 620)
                          height: parent.height - 180
                          radius: 6
                          color: "${ca "base01" "f2"}"
                          border.width: 1
                          border.color: "${c "base02"}"

                          // Soft outer "Aero" glow
                          Rectangle {
                              anchors.fill: parent
                              anchors.margins: -1
                              radius: parent.radius + 1
                              color: "transparent"
                              border.width: 1
                              border.color: "${ca "base0D" "33"}"
                              z: -1
                          }

                          ColumnLayout {
                              anchors.fill: parent
                              spacing: 0

                              // ── Title bar ─────────────────────
                              Rectangle {
                                  id: titleBar
                                  Layout.fillWidth: true
                                  Layout.preferredHeight: 30
                                  radius: 6
                                  gradient: Gradient {
                                      GradientStop { position: 0.0; color: "${ca "base02" "ee"}" }
                                      GradientStop { position: 1.0; color: "${ca "base01" "ee"}" }
                                  }

                                  Rectangle {
                                      anchors.left: parent.left
                                      anchors.right: parent.right
                                      anchors.bottom: parent.bottom
                                      height: parent.radius
                                      color: "${ca "base01" "ee"}"
                                  }

                                  Text {
                                      anchors.left: parent.left
                                      anchors.leftMargin: 12
                                      anchors.verticalCenter: parent.verticalCenter
                                      text: "󰇮  Outlook — Calendar — Supreme Scrumlord Merrinx"
                                      color: "${c "base05"}"
                                      font.family: "Segoe UI"
                                      font.pixelSize: 12
                                  }

                                  Row {
                                      anchors.right: parent.right
                                      anchors.top: parent.top
                                      spacing: 0

                                      Rectangle {
                                          width: 46; height: 22
                                          color: "transparent"
                                          Text {
                                              anchors.centerIn: parent
                                              anchors.verticalCenterOffset: -4
                                              text: "─"
                                              color: "${c "base05"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 14
                                          }
                                      }
                                      Rectangle {
                                          width: 46; height: 22
                                          color: "transparent"
                                          Rectangle {
                                              anchors.centerIn: parent
                                              width: 10; height: 9
                                              color: "transparent"
                                              border.width: 1
                                              border.color: "${c "base05"}"
                                          }
                                      }
                                      Rectangle {
                                          width: 50; height: 22
                                          radius: 2
                                          color: "${ca "base08" "55"}"
                                          Text {
                                              anchors.centerIn: parent
                                              text: "✕"
                                              color: "${c "base05"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 12
                                              font.bold: true
                                          }
                                      }
                                  }
                              }

                              // ── Menu bar ──────────────────────
                              Rectangle {
                                  Layout.fillWidth: true
                                  Layout.preferredHeight: 24
                                  color: "${ca "base01" "cc"}"
                                  Row {
                                      anchors.left: parent.left
                                      anchors.leftMargin: 8
                                      anchors.verticalCenter: parent.verticalCenter
                                      spacing: 16
                                      Repeater {
                                          model: ["File", "Edit", "View", "Go", "Tools", "Help"]
                                          Text {
                                              text: modelData
                                              color: "${c "base04"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 11
                                          }
                                      }
                                  }
                              }

                              Rectangle {
                                  Layout.fillWidth: true
                                  Layout.preferredHeight: 1
                                  color: "${c "base02"}"
                              }

                              // ── Content ───────────────────────
                              Rectangle {
                                  Layout.fillWidth: true
                                  Layout.fillHeight: true
                                  color: "${ca "base00" "cc"}"

                                  ColumnLayout {
                                      anchors.fill: parent
                                      anchors.margins: 16
                                      spacing: 10

                                      RowLayout {
                                          Layout.fillWidth: true
                                          Text {
                                              text: "󰃭  Today's Synergy"
                                              color: "${c "base0D"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 16
                                              font.weight: Font.DemiBold
                                          }
                                          Item { Layout.fillWidth: true }
                                          Text {
                                              id: calToday
                                              text: ""
                                              color: "${c "base04"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 12
                                              Timer {
                                                  running: true; repeat: true; interval: 30000; triggeredOnStart: true
                                                  onTriggered: calToday.text =
                                                      Qt.formatDateTime(new Date(), "dddd, MMMM d")
                                              }
                                          }
                                      }

                                      Rectangle {
                                          Layout.fillWidth: true
                                          height: 1
                                          color: "${c "base02"}"
                                      }

                                      ListView {
                                          Layout.fillWidth: true
                                          Layout.fillHeight: true
                                          clip: true
                                          interactive: false
                                          spacing: 5
                                          model: [
                                              { time: "08:30", title: "Daily Standup",                      tag: "SCRUM"   },
                                              { time: "09:15", title: "Pre-Standup Sync (re: standup)",     tag: "MEETING" },
                                              { time: "10:00", title: "Rust Rewrite Steering Committee",    tag: "RUST"    },
                                              { time: "11:00", title: "JIRA Grooming Marathon",             tag: "JIRA"    },
                                              { time: "12:00", title: "Lunch & Learn: Estimates Are Lies",  tag: "LEARN"   },
                                              { time: "13:00", title: "Retro: Why Prod Is On Fire (Again)", tag: "RETRO"   },
                                              { time: "14:00", title: "Cross-Functional Synergy Alignment", tag: "SYNERGY" },
                                              { time: "15:00", title: "AI-First Re-imagining of 'cat'",     tag: "AI"      },
                                              { time: "16:00", title: "Backlog Triage → Won't Fix",         tag: "JIRA"    },
                                              { time: "17:00", title: "Mandatory Optional Team Building",   tag: "FUN!"    }
                                          ]
                                          delegate: Rectangle {
                                              width: ListView.view.width
                                              height: 38
                                              color: "${ca "base01" "99"}"
                                              radius: 3
                                              border.width: 1
                                              border.color: "${c "base02"}"

                                              Rectangle {
                                                  width: 3
                                                  anchors.top: parent.top
                                                  anchors.bottom: parent.bottom
                                                  anchors.left: parent.left
                                                  color: "${c "base0D"}"
                                                  radius: 3
                                              }

                                              RowLayout {
                                                  anchors.fill: parent
                                                  anchors.leftMargin: 14
                                                  anchors.rightMargin: 10
                                                  spacing: 10

                                                  Text {
                                                      text: modelData.time
                                                      color: "${c "base0C"}"
                                                      font.family: "Segoe UI"
                                                      font.pixelSize: 12
                                                      font.weight: Font.Bold
                                                  }
                                                  Rectangle {
                                                      Layout.preferredWidth: 1
                                                      Layout.fillHeight: true
                                                      Layout.topMargin: 8
                                                      Layout.bottomMargin: 8
                                                      color: "${c "base02"}"
                                                  }
                                                  Text {
                                                      Layout.fillWidth: true
                                                      text: modelData.title
                                                      color: "${c "base05"}"
                                                      font.family: "Segoe UI"
                                                      font.pixelSize: 12
                                                      elide: Text.ElideRight
                                                  }
                                                  Rectangle {
                                                      Layout.preferredHeight: 16
                                                      Layout.preferredWidth: tagText.implicitWidth + 12
                                                      radius: 3
                                                      color: "${ca "base0E" "33"}"
                                                      border.width: 1
                                                      border.color: "${ca "base0E" "66"}"
                                                      Text {
                                                          id: tagText
                                                          anchors.centerIn: parent
                                                          text: modelData.tag
                                                          color: "${c "base0E"}"
                                                          font.family: "Segoe UI"
                                                          font.pixelSize: 9
                                                          font.weight: Font.Bold
                                                      }
                                                  }
                                              }
                                          }
                                      }

                                      Text {
                                          Layout.fillWidth: true
                                          horizontalAlignment: Text.AlignHCenter
                                          text: "+ 6 unscheduled \"quick chats\""
                                          color: "${c "base04"}"
                                          font.family: "Segoe UI"
                                          font.pixelSize: 11
                                          font.italic: true
                                      }
                                  }
                              }

                              // ── Status bar ────────────────────
                              Rectangle {
                                  Layout.fillWidth: true
                                  Layout.preferredHeight: 22
                                  color: "${ca "base02" "cc"}"
                                  Text {
                                      anchors.left: parent.left
                                      anchors.leftMargin: 10
                                      anchors.verticalCenter: parent.verticalCenter
                                      text: "10 items · synced with Stakeholder Cloud™"
                                      color: "${c "base04"}"
                                      font.family: "Segoe UI"
                                      font.pixelSize: 10
                                  }
                              }
                          }
                      }

                      // ╔════════════════════════════════════════╗
                      // ║  LOGIN TILE  (centre-right)            ║
                      // ╚════════════════════════════════════════╝
                      Item {
                          id: loginTile
                          width: 360
                          height: 360
                          anchors.verticalCenter: parent.verticalCenter
                          anchors.verticalCenterOffset: -40
                          x: Math.max(calWindow.x + calWindow.width + 80,
                                      parent.width / 2 + (parent.width / 2 - width) / 2)

                          ColumnLayout {
                              anchors.fill: parent
                              spacing: 14

                              Rectangle {
                                  Layout.alignment: Qt.AlignHCenter
                                  Layout.preferredWidth: 140
                                  Layout.preferredHeight: 140
                                  radius: 6
                                  color: "${ca "base0D" "33"}"
                                  border.width: 2
                                  border.color: "${c "base0D"}"

                                  ColumnLayout {
                                      anchors.centerIn: parent
                                      spacing: 2
                                      Text {
                                          Layout.alignment: Qt.AlignHCenter
                                          text: "󰮊"
                                          color: "${c "base0A"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 44
                                      }
                                      Text {
                                          Layout.alignment: Qt.AlignHCenter
                                          text: "SSM"
                                          color: "${c "base05"}"
                                          font.family: "Segoe UI"
                                          font.pixelSize: 28
                                          font.weight: Font.Light
                                          font.letterSpacing: 4
                                      }
                                  }
                              }

                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  text: "Supreme Scrumlord Merrinx"
                                  color: "${c "base05"}"
                                  font.family: "Segoe UI"
                                  font.pixelSize: 18
                                  font.weight: Font.Light
                              }

                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  text: "Administrator · TPS-Reports.local"
                                  color: "${c "base04"}"
                                  font.family: "Segoe UI"
                                  font.pixelSize: 11
                                  font.italic: true
                              }

                              // ── Password field (Win7 style) ───
                              Rectangle {
                                  id: pwBox
                                  Layout.alignment: Qt.AlignHCenter
                                  Layout.preferredWidth: 280
                                  Layout.preferredHeight: 30
                                  radius: 2
                                  color: "${ca "base00" "f2"}"
                                  border.width: 1
                                  border.color: root.authFailed
                                      ? "${c "base08"}"
                                      : root.authenticating
                                          ? "${c "base0A"}"
                                          : "${c "base0D"}"

                                  Behavior on border.color {
                                      ColorAnimation { duration: 200 }
                                  }

                                  SequentialAnimation on x {
                                      id: shakeAnim
                                      running: false
                                      loops: 1
                                      NumberAnimation { from: 0;    to: -10; duration: 50 }
                                      NumberAnimation { from: -10;  to:  10; duration: 50 }
                                      NumberAnimation { from:  10;  to: -8;  duration: 50 }
                                      NumberAnimation { from: -8;   to:  8;  duration: 50 }
                                      NumberAnimation { from:  8;   to:  0;  duration: 50 }
                                  }

                                  TextInput {
                                      id: passwordInput
                                      anchors.fill: parent
                                      anchors.leftMargin: 10
                                      anchors.rightMargin: 36
                                      verticalAlignment: TextInput.AlignVCenter
                                      color: "${c "base05"}"
                                      font.family: "Segoe UI"
                                      font.pixelSize: 13
                                      echoMode: TextInput.Password
                                      enabled: !root.authenticating
                                      focus: true

                                      text: root.password
                                      onTextEdited: root.password = text

                                      Keys.onEnterPressed: root.tryAuth()
                                      Keys.onReturnPressed: root.tryAuth()
                                  }

                                  Text {
                                      visible: passwordInput.text === "" && !root.authenticating
                                      anchors.left: parent.left
                                      anchors.leftMargin: 10
                                      anchors.verticalCenter: parent.verticalCenter
                                      text: "Password"
                                      color: "${c "base04"}"
                                      font.family: "Segoe UI"
                                      font.pixelSize: 13
                                      font.italic: true
                                  }

                                  // Submit arrow
                                  Rectangle {
                                      anchors.right: parent.right
                                      anchors.top: parent.top
                                      anchors.bottom: parent.bottom
                                      anchors.margins: 2
                                      width: 28
                                      radius: 2
                                      color: passwordInput.text === ""
                                          ? "${ca "base02" "99"}"
                                          : "${c "base0D"}"

                                      Behavior on color {
                                          ColorAnimation { duration: 150 }
                                      }

                                      Text {
                                          anchors.centerIn: parent
                                          text: "→"
                                          color: passwordInput.text === ""
                                              ? "${c "base04"}"
                                              : "${c "base00"}"
                                          font.family: "Segoe UI"
                                          font.pixelSize: 14
                                          font.bold: true
                                      }
                                      MouseArea {
                                          anchors.fill: parent
                                          cursorShape: Qt.PointingHandCursor
                                          onClicked: root.tryAuth()
                                      }
                                  }
                              }

                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  visible: root.authFailed
                                  text: "The user name or password is incorrect."
                                  color: "${c "base08"}"
                                  font.family: "Segoe UI"
                                  font.pixelSize: 11
                              }

                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  visible: root.authenticating
                                  text: "Awaiting stakeholder approval…"
                                  color: "${c "base0A"}"
                                  font.family: "Segoe UI"
                                  font.pixelSize: 11
                                  font.italic: true
                              }

                              Text {
                                  Layout.alignment: Qt.AlignHCenter
                                  text: "Switch User    |    Ease of access"
                                  color: "${c "base04"}"
                                  font.family: "Segoe UI"
                                  font.pixelSize: 10
                              }
                          }
                      }

                      // ╔════════════════════════════════════════╗
                      // ║  NOTIFICATION TOASTS (bottom-right)    ║
                      // ╚════════════════════════════════════════╝
                      Column {
                          id: notifColumn
                          anchors.right: parent.right
                          anchors.rightMargin: 16
                          anchors.bottom: parent.bottom
                          anchors.bottomMargin: 60
                          spacing: 10

                          Repeater {
                              model: [
                                  {
                                      sender:   "Donald J. Trump",
                                      time:     "2 min ago",
                                      subject:  "TREMENDOUS sprint velocity (the BEST people are saying)",
                                      preview:  "Folks, I just spoke with the best devs, frankly the BEST, and they tell me your burndown chart is going UP — many people don't know this, but UP is the new DOWN…",
                                      accent:   "${c "base08"}"
                                  },
                                  {
                                      sender:   "H.M. King Harald V of Norway",
                                      time:     "11 min ago",
                                      subject:  "Royal Inquiry — Sprint #142, In Perpetuity",
                                      preview:  "His Majesty graciously requests an audience regarding the eternal sprint. The Royal Court notes with mild concern that the burndown remains stubbornly an 'up-burn'.",
                                      accent:   "${c "base0E"}"
                                  },
                                  {
                                      sender:   "Richard M. Stallman",
                                      time:     "37 min ago",
                                      subject:  "I'd just like to interject for a moment…",
                                      preview:  "What you are calling Windows is in fact GNU/Emacs, or as I've recently taken to calling it, Emacs-the-Operating-System-which-happens-to-include-a-text-editor. Please reboot into Emacs.",
                                      accent:   "${c "base0B"}"
                                  }
                              ]
                              delegate: Rectangle {
                                  width: 360
                                  height: bodyCol.implicitHeight + 22
                                  radius: 4
                                  color: "${ca "base01" "f2"}"
                                  border.width: 1
                                  border.color: "${c "base02"}"

                                  Rectangle {
                                      anchors.left: parent.left
                                      anchors.top: parent.top
                                      anchors.bottom: parent.bottom
                                      width: 4
                                      color: modelData.accent
                                      radius: 2
                                  }

                                  Rectangle {
                                      anchors.top: parent.top
                                      anchors.left: parent.left
                                      anchors.right: parent.right
                                      height: 1
                                      color: "${ca "base05" "22"}"
                                  }

                                  ColumnLayout {
                                      id: bodyCol
                                      anchors.left: parent.left
                                      anchors.right: parent.right
                                      anchors.top: parent.top
                                      anchors.leftMargin: 14
                                      anchors.rightMargin: 24
                                      anchors.topMargin: 9
                                      spacing: 2

                                      RowLayout {
                                          Layout.fillWidth: true
                                          spacing: 6
                                          Text {
                                              text: "󰇮"
                                              color: modelData.accent
                                              font.family: "RobotoMono Nerd Font"
                                              font.pixelSize: 13
                                          }
                                          Text {
                                              text: modelData.sender
                                              color: "${c "base05"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 12
                                              font.weight: Font.DemiBold
                                              elide: Text.ElideRight
                                              Layout.fillWidth: true
                                          }
                                          Text {
                                              text: modelData.time
                                              color: "${c "base04"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 10
                                          }
                                      }

                                      Text {
                                          Layout.fillWidth: true
                                          text: modelData.subject
                                          color: "${c "base05"}"
                                          font.family: "Segoe UI"
                                          font.pixelSize: 11
                                          font.weight: Font.Medium
                                          elide: Text.ElideRight
                                      }

                                      Text {
                                          Layout.fillWidth: true
                                          text: modelData.preview
                                          color: "${c "base04"}"
                                          font.family: "Segoe UI"
                                          font.pixelSize: 10
                                          wrapMode: Text.WordWrap
                                          maximumLineCount: 2
                                          elide: Text.ElideRight
                                      }
                                  }

                                  Text {
                                      anchors.right: parent.right
                                      anchors.top: parent.top
                                      anchors.rightMargin: 8
                                      anchors.topMargin: 4
                                      text: "✕"
                                      color: "${c "base04"}"
                                      font.family: "Segoe UI"
                                      font.pixelSize: 10
                                  }
                              }
                          }
                      }

                      // ╔════════════════════════════════════════╗
                      // ║  TASKBAR (bottom)                      ║
                      // ╚════════════════════════════════════════╝
                      Rectangle {
                          id: taskbar
                          anchors.left: parent.left
                          anchors.right: parent.right
                          anchors.bottom: parent.bottom
                          height: 40
                          gradient: Gradient {
                              GradientStop { position: 0.0; color: "${ca "base01" "ee"}" }
                              GradientStop { position: 1.0; color: "${ca "base00" "ee"}" }
                          }
                          border.width: 1
                          border.color: "${c "base02"}"

                          Rectangle {
                              anchors.top: parent.top
                              anchors.left: parent.left
                              anchors.right: parent.right
                              height: 1
                              color: "${ca "base0D" "55"}"
                          }

                          // ── Start orb ────────────────────────
                          Rectangle {
                              id: startOrb
                              anchors.left: parent.left
                              anchors.leftMargin: 4
                              anchors.verticalCenter: parent.verticalCenter
                              width: 44; height: 32
                              radius: 16
                              gradient: Gradient {
                                  GradientStop { position: 0.0; color: "${c "base0D"}" }
                                  GradientStop { position: 1.0; color: "${c "base0C"}" }
                              }
                              border.width: 1
                              border.color: "${ca "base05" "55"}"

                              Text {
                                  anchors.centerIn: parent
                                  text: "󰖳"
                                  color: "${c "base00"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 18
                              }
                          }

                          // ── Pinned / running apps ────────────
                          Row {
                              anchors.left: startOrb.right
                              anchors.leftMargin: 8
                              anchors.verticalCenter: parent.verticalCenter
                              spacing: 4

                              Repeater {
                                  model: [
                                      { icon: "󰇮", color: "${c "base0D"}", active: true  },
                                      { icon: "󰈹", color: "${c "base0E"}", active: false },
                                      { icon: "󰉋", color: "${c "base0A"}", active: false },
                                      { icon: "󰈙", color: "${c "base09"}", active: false },
                                      { icon: "󰓪", color: "${c "base0B"}", active: false },
                                      { icon: "󰆍", color: "${c "base04"}", active: false }
                                  ]
                                  Rectangle {
                                      width: 40; height: 32
                                      radius: 3
                                      color: modelData.active
                                          ? "${ca "base02" "cc"}"
                                          : "transparent"
                                      border.width: modelData.active ? 1 : 0
                                      border.color: "${ca "base0D" "66"}"

                                      Text {
                                          anchors.centerIn: parent
                                          text: modelData.icon
                                          color: modelData.color
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 18
                                      }
                                  }
                              }
                          }

                          // ── System tray + clock ──────────────
                          Row {
                              anchors.right: parent.right
                              anchors.verticalCenter: parent.verticalCenter
                              anchors.rightMargin: 8
                              spacing: 12

                              Row {
                                  spacing: 8
                                  anchors.verticalCenter: parent.verticalCenter
                                  Text {
                                      text: "󰖩"; color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                  }
                                  Text {
                                      text: "󰕾"; color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                  }
                                  Text {
                                      text: "󰂄"; color: "${c "base0B"}"
                                      font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                  }
                                  Item {
                                      width: 18; height: 18
                                      Text {
                                          anchors.centerIn: parent
                                          text: "󰇰"; color: "${c "base0A"}"
                                          font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                      }
                                      Rectangle {
                                          anchors.right: parent.right
                                          anchors.top: parent.top
                                          width: 12; height: 10
                                          radius: 5
                                          color: "${c "base08"}"
                                          Text {
                                              anchors.centerIn: parent
                                              text: "3"
                                              color: "${c "base00"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 8
                                              font.bold: true
                                          }
                                      }
                                  }
                              }

                              Rectangle {
                                  width: 1; height: 24
                                  anchors.verticalCenter: parent.verticalCenter
                                  color: "${c "base02"}"
                              }

                              Column {
                                  anchors.verticalCenter: parent.verticalCenter
                                  spacing: 0
                                  Text {
                                      id: trayTime
                                      text: ""
                                      color: "${c "base05"}"
                                      font.family: "Segoe UI"
                                      font.pixelSize: 12
                                      horizontalAlignment: Text.AlignRight
                                      width: parent.width
                                      Timer {
                                          running: true; repeat: true; interval: 1000; triggeredOnStart: true
                                          onTriggered: trayTime.text =
                                              Qt.formatDateTime(new Date(), "HH:mm")
                                      }
                                  }
                                  Text {
                                      id: trayDate
                                      text: ""
                                      color: "${c "base04"}"
                                      font.family: "Segoe UI"
                                      font.pixelSize: 10
                                      horizontalAlignment: Text.AlignRight
                                      width: parent.width
                                      Timer {
                                          running: true; repeat: true; interval: 30000; triggeredOnStart: true
                                          onTriggered: trayDate.text =
                                              Qt.formatDateTime(new Date(), "ddd, MMM d")
                                      }
                                  }
                              }

                              Rectangle {
                                  width: 8; height: 32
                                  anchors.verticalCenter: parent.verticalCenter
                                  radius: 2
                                  color: "${ca "base02" "aa"}"
                                  border.width: 1
                                  border.color: "${c "base02"}"
                              }
                          }
                      }
                  }
              }
          }
      }
  }
''
