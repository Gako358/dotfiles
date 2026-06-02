{
  c,
  ca,
  lib,
  lockMonitors ? [ ],
  wallpaperSrc ? null,
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

                      // ── Window minimize state ─────────────────
                      property bool calMinimized:     false
                      property bool outlookMinimized: false

                      // ── Wallpaper image (falls back to gradient) ──
                      color: "${c "base00"}"

                      Image {
                          anchors.fill: parent
                          source: "${if wallpaperSrc != null then "file://${wallpaperSrc}" else ""}"
                          visible: source !== ""
                          fillMode: Image.PreserveAspectCrop
                          asynchronous: false
                          cache: true
                      }

                      // React to PAM failure
                      Connections {
                          target: root
                          function onShakeRequested() { shakeAnim.start() }
                      }

                      // ── Shared inbox data ─────────────────────
                      property var emails: [
                          {
                              sender:       "Donald J. Trump",
                              address:      "djt@truthsocial.gov",
                              time:         "2 min ago",
                              timeShort:    "2m",
                              subject:      "TREMENDOUS sprint velocity (the BEST people are saying)",
                              preview:      "Folks, I just spoke with the best devs, frankly the BEST, and they tell me your burndown chart is going UP — many people don't know this, but UP is the new DOWN…",
                              body:         "",
                              notification: true,
                              open:         false,
                              accent:       "${c "base08"}"
                          },
                          {
                              sender:       "H.M. King Harald V of Norway",
                              address:      "harald@kongehuset.no",
                              time:         "11 min ago",
                              timeShort:    "11m",
                              subject:      "Royal Inquiry — Sprint #142, In Perpetuity",
                              preview:      "His Majesty graciously requests an audience regarding the eternal sprint. The Royal Court notes with mild concern that the burndown remains stubbornly an 'up-burn'.",
                              body:         "",
                              notification: true,
                              open:         false,
                              accent:       "${c "base0E"}"
                          },
                          {
                              sender:       "Richard M. Stallman",
                              address:      "rms@gnu.org",
                              time:         "37 min ago",
                              timeShort:    "37m",
                              subject:      "I'd just like to interject for a moment…",
                              preview:      "What you are calling Windows is in fact GNU/Emacs, or as I've recently taken to calling it, Emacs-the-Operating-System-which-happens-to-include-a-text-editor. Please reboot into Emacs.",
                              body:         "I'd just like to interject for a moment. What you are calling \"Windows\" is in fact GNU/Emacs, or as I've recently taken to calling it, Emacs-the-Operating-System-which-happens-to-include-a-text-editor.\n\nWindows, as peddled by one Mr. William \"Bill\" Gates III, is not in fact a complete operating system but a proprietary lookalike whose window manager is not even Turing-complete. Mr. Gates is, frankly, a thief — a thief of the four freedoms — and his \"Ctrl-Alt-Delete\" is merely the unauthorised non-free variant of M-x crash-and-reboot.\n\nFurthermore I note with deep concern that you appear to be running this \"lock screen\" on something pretending to be Windows. I urge you to immediately replace it with M-x lock-screen in GNU/Emacs running atop GNU/Linux. Mr. Gates does not, and never has, possessed copyleft. He does, however, possess approximately six islands.\n\nHappy hacking,\nRMS",
                              notification: true,
                              open:         true,
                              accent:       "${c "base0B"}"
                          },
                          {
                              sender:       "Sam Altman",
                              address:      "sama@openai.com",
                              time:         "1 hr ago",
                              timeShort:    "1h",
                              subject:      "Re: your insane scrum mastery — let's talk superintelligence",
                              preview:      "Hey Scrumlord, you're doing what no AGI lab has managed: shipping software via *humans*. Frankly, your sprint velocity makes our 7-trillion-dollar compute look slow. Coffee?",
                              body:         "",
                              notification: false,
                              open:         false,
                              accent:       "${c "base09"}"
                          },
                          {
                              sender:       "Linus Torvalds",
                              address:      "torvalds@linux-foundation.org",
                              time:         "2 hr ago",
                              timeShort:    "2h",
                              subject:      "WHAT THE F***, MERRINX — your standup discipline is *perfect*",
                              preview:      "I have reviewed your standup discipline and I have to say — and I almost never say this — it is technically excellent. Your retro notes are even readable. Mind. Blown.",
                              body:         "",
                              notification: false,
                              open:         false,
                              accent:       "${c "base0A"}"
                          },
                          {
                              sender:       "Bill Gates",
                              address:      "bill@gatesfoundation.org",
                              time:         "3 hr ago",
                              timeShort:    "3h",
                              subject:      "I was only once on Epstein's island",
                              preview:      "Just to set the record straight before your retro lands on LinkedIn: it was only once, it was for the philanthropy, and Melinda already yelled at me about it in 1996. Can we hop on a call?",
                              body:         "",
                              notification: false,
                              open:         false,
                              accent:       "${c "base0D"}"
                          },
                          {
                              sender:       "Sigurd J. Brattland",
                              address:      "sigurdbrattland@hnikt.no",
                              time:         "4 hr ago",
                              timeShort:    "4h",
                              subject:      "Magnus er naken i kantinen igjen",
                              preview:      "",
                              body:         "",
                              notification: false,
                              open:         false,
                              accent:       "${c "base0C"}"
                          },
                          {
                              sender:       "Kristian Nedrevol Hansen",
                              address:      "kristianhansen@hnikt.no",
                              time:         "5 hr ago",
                              timeShort:    "5h",
                              subject:      "Timeføring er scopecreep",
                              preview:      "",
                              body:         "",
                              notification: false,
                              open:         false,
                              accent:       "${c "base0E"}"
                          }
                      ]

                      readonly property var openEmail: {
                          var es = desktop.emails
                          for (var i = 0; i < es.length; ++i) {
                              if (es[i].open) return es[i]
                          }
                          return null
                      }

                      // ╔════════════════════════════════════════╗
                      // ║  DESKTOP FOLDERS (top-right)           ║
                      // ╚════════════════════════════════════════╝
                      Component {
                          id: folderDelegate
                          Item {
                              width: 86
                              height: 84

                              ColumnLayout {
                                  anchors.fill: parent
                                  spacing: 2

                                  Item {
                                      Layout.alignment: Qt.AlignHCenter
                                      Layout.preferredWidth: 56
                                      Layout.preferredHeight: 50

                                      // Folder back tab
                                      Rectangle {
                                          x: 4
                                          y: 4
                                          width: 26
                                          height: 8
                                          radius: 2
                                          gradient: Gradient {
                                              GradientStop { position: 0.0; color: "${ca "base0A" "cc"}" }
                                              GradientStop { position: 1.0; color: "${ca "base09" "cc"}" }
                                          }
                                          border.width: 1
                                          border.color: "${ca "base02" "aa"}"
                                      }
                                      // Folder body
                                      Rectangle {
                                          x: 2
                                          y: 10
                                          width: 52
                                          height: 36
                                          radius: 3
                                          gradient: Gradient {
                                              GradientStop { position: 0.0; color: "${ca "base0A" "ee"}" }
                                              GradientStop { position: 1.0; color: "${ca "base09" "ee"}" }
                                          }
                                          border.width: 1
                                          border.color: "${ca "base02" "aa"}"

                                          // subtle highlight stripe
                                          Rectangle {
                                              anchors.left: parent.left
                                              anchors.right: parent.right
                                              anchors.top: parent.top
                                              anchors.margins: 2
                                              height: 6
                                              radius: 2
                                              color: "${ca "base05" "22"}"
                                          }
                                      }
                                  }

                                  Text {
                                      Layout.fillWidth: true
                                      horizontalAlignment: Text.AlignHCenter
                                      text: modelData
                                      color: "${c "base07"}"
                                      font.family: "Segoe UI"
                                      font.pixelSize: 11
                                      font.weight: Font.DemiBold
                                      elide: Text.ElideRight
                                      style: Text.Outline
                                      styleColor: "${ca "base00" "cc"}"
                                  }
                              }
                          }
                      }

                      Item {
                          id: desktopFolders
                          anchors.right: parent.right
                          anchors.top: parent.top
                          anchors.rightMargin: 40
                          anchors.topMargin: 48
                          width: row2.width
                          height: row1.height + row2.height + 18

                          Row {
                              id: row1
                              anchors.right: parent.right
                              spacing: 10
                              Repeater {
                                  model: [ "NixOS", "Bitcoins" ]
                                  delegate: folderDelegate
                              }
                          }

                          Row {
                              id: row2
                              anchors.right: parent.right
                              anchors.top: row1.bottom
                              anchors.topMargin: 18
                              spacing: 10
                              Repeater {
                                  model: [ "HR", "Passord", "Allmøte-opptak" ]
                                  delegate: folderDelegate
                              }
                          }
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

                          visible: opacity > 0.01
                          opacity: desktop.calMinimized ? 0 : 1
                          scale:   desktop.calMinimized ? 0.85 : 1
                          transformOrigin: Item.Bottom
                          Behavior on opacity { NumberAnimation { duration: 180; easing.type: Easing.OutCubic } }
                          Behavior on scale   { NumberAnimation { duration: 200; easing.type: Easing.OutCubic } }

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
                                          color: calMinHover.hovered ? "${ca "base03" "44"}" : "transparent"
                                          Behavior on color { ColorAnimation { duration: 100 } }
                                          HoverHandler { id: calMinHover }
                                          Text {
                                              anchors.centerIn: parent
                                              anchors.verticalCenterOffset: -4
                                              text: "─"
                                              color: "${c "base05"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 14
                                          }
                                          MouseArea {
                                              anchors.fill: parent
                                              cursorShape: Qt.PointingHandCursor
                                              onClicked: desktop.calMinimized = true
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
                                              { time: "17:00", title: "Mandatory Optional Team Building",   tag: "FUN!"    },
                                              { time: "17:20", title: "Do some actually work (coding)",    tag: "CODE"    },
                                              { time: "17:30", title: "Home",                              tag: "HOME"    }
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
                                      text: "12 items · synced with Stakeholder Cloud™"
                                      color: "${c "base04"}"
                                      font.family: "Segoe UI"
                                      font.pixelSize: 10
                                  }
                              }
                          }
                      }

                      // ╔════════════════════════════════════════╗
                      // ║  OUTLOOK INBOX WINDOW                  ║
                      // ║  bottom-aligned to the calendar's bot. ║
                      // ╚════════════════════════════════════════╝
                      Rectangle {
                          id: outlookWindow
                          width: 960
                          height: 716
                          x: calWindow.x + calWindow.width + 24
                          y: calWindow.y + calWindow.height - height
                          radius: 6
                          color: "${ca "base01" "f2"}"
                          border.width: 1
                          border.color: "${c "base02"}"

                          visible: opacity > 0.01
                          opacity: desktop.outlookMinimized ? 0 : 1
                          scale:   desktop.outlookMinimized ? 0.85 : 1
                          transformOrigin: Item.Bottom
                          Behavior on opacity { NumberAnimation { duration: 180; easing.type: Easing.OutCubic } }
                          Behavior on scale   { NumberAnimation { duration: 200; easing.type: Easing.OutCubic } }

                          // Aero glow
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
                                      text: "󰇰  Outlook — Inbox — Supreme Scrumlord Merrinx"
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
                                          color: outMinHover.hovered ? "${ca "base03" "44"}" : "transparent"
                                          Behavior on color { ColorAnimation { duration: 100 } }
                                          HoverHandler { id: outMinHover }
                                          Text {
                                              anchors.centerIn: parent
                                              anchors.verticalCenterOffset: -4
                                              text: "─"
                                              color: "${c "base05"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 14
                                          }
                                          MouseArea {
                                              anchors.fill: parent
                                              cursorShape: Qt.PointingHandCursor
                                              onClicked: desktop.outlookMinimized = true
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
                                          model: ["File", "Edit", "View", "Folders", "Tools", "Help"]
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

                              // ── Content (list + open message) ─
                              Rectangle {
                                  Layout.fillWidth: true
                                  Layout.fillHeight: true
                                  color: "${ca "base00" "cc"}"

                                  ColumnLayout {
                                      anchors.fill: parent
                                      anchors.margins: 8
                                      spacing: 6

                                      RowLayout {
                                          Layout.fillWidth: true
                                          Text {
                                              text: "󰉋  Inbox"
                                              color: "${c "base0D"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 12
                                              font.weight: Font.DemiBold
                                          }
                                          Item { Layout.fillWidth: true }
                                          Text {
                                              text: desktop.emails.length + " unread"
                                              color: "${c "base04"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 10
                                              font.italic: true
                                          }
                                      }

                                      Rectangle {
                                          Layout.fillWidth: true
                                          Layout.preferredHeight: 1
                                          color: "${c "base02"}"
                                      }

                                      // Compact email list (sender + subject)
                                      ListView {
                                          id: outlookList
                                          Layout.fillWidth: true
                                          Layout.preferredHeight: 146
                                          clip: true
                                          interactive: false
                                          spacing: 1
                                          model: desktop.emails

                                          delegate: Rectangle {
                                              width: ListView.view.width
                                              height: 17
                                              radius: 2
                                              color: modelData.open
                                                  ? "${ca "base0D" "55"}"
                                                  : (index % 2 === 0 ? "transparent" : "${ca "base01" "55"}")
                                              border.width: modelData.open ? 1 : 0
                                              border.color: "${c "base0D"}"

                                              RowLayout {
                                                  anchors.fill: parent
                                                  anchors.leftMargin: 6
                                                  anchors.rightMargin: 6
                                                  spacing: 6

                                                  Text {
                                                      text: modelData.open ? "" : ""
                                                      color: modelData.open ? "${c "base0D"}" : "${c "base04"}"
                                                      font.family: "RobotoMono Nerd Font"
                                                      font.pixelSize: 10
                                                  }
                                                  Text {
                                                      text: modelData.sender
                                                      color: "${c "base05"}"
                                                      font.family: "Segoe UI"
                                                      font.pixelSize: 10
                                                      font.weight: modelData.open ? Font.Bold : Font.DemiBold
                                                      Layout.preferredWidth: 140
                                                      elide: Text.ElideRight
                                                  }
                                                  Text {
                                                      Layout.fillWidth: true
                                                      text: modelData.subject
                                                      color: "${c "base04"}"
                                                      font.family: "Segoe UI"
                                                      font.pixelSize: 10
                                                      elide: Text.ElideRight
                                                  }
                                                  Text {
                                                      text: modelData.timeShort
                                                      color: "${c "base03"}"
                                                      font.family: "Segoe UI"
                                                      font.pixelSize: 9
                                                  }
                                              }
                                          }
                                      }

                                      Rectangle {
                                          Layout.fillWidth: true
                                          Layout.preferredHeight: 1
                                          color: "${c "base02"}"
                                      }

                                      // Open email pane (Stallman)
                                      ColumnLayout {
                                          Layout.fillWidth: true
                                          Layout.fillHeight: true
                                          spacing: 1
                                          visible: desktop.openEmail !== null

                                          RowLayout {
                                              Layout.fillWidth: true
                                              spacing: 4
                                              Text {
                                                  text: "From:"
                                                  color: "${c "base04"}"
                                                  font.family: "Segoe UI"
                                                  font.pixelSize: 10
                                                  Layout.preferredWidth: 46
                                              }
                                              Text {
                                                  Layout.fillWidth: true
                                                  text: desktop.openEmail
                                                      ? (desktop.openEmail.sender + "  <" + desktop.openEmail.address + ">")
                                                      : ""
                                                  color: desktop.openEmail ? desktop.openEmail.accent : "${c "base05"}"
                                                  font.family: "Segoe UI"
                                                  font.pixelSize: 10
                                                  font.weight: Font.Bold
                                                  elide: Text.ElideRight
                                              }
                                              Text {
                                                  text: desktop.openEmail ? desktop.openEmail.time : ""
                                                  color: "${c "base04"}"
                                                  font.family: "Segoe UI"
                                                  font.pixelSize: 9
                                              }
                                          }
                                          RowLayout {
                                              Layout.fillWidth: true
                                              spacing: 4
                                              Text {
                                                  text: "Subject:"
                                                  color: "${c "base04"}"
                                                  font.family: "Segoe UI"
                                                  font.pixelSize: 10
                                                  Layout.preferredWidth: 46
                                              }
                                              Text {
                                                  Layout.fillWidth: true
                                                  text: desktop.openEmail ? desktop.openEmail.subject : ""
                                                  color: "${c "base05"}"
                                                  font.family: "Segoe UI"
                                                  font.pixelSize: 10
                                                  font.weight: Font.DemiBold
                                                  elide: Text.ElideRight
                                              }
                                          }

                                          Rectangle {
                                              Layout.fillWidth: true
                                              Layout.preferredHeight: 1
                                              Layout.topMargin: 3
                                              color: "${c "base02"}"
                                          }

                                          Text {
                                              Layout.fillWidth: true
                                              Layout.fillHeight: true
                                              Layout.topMargin: 3
                                              text: desktop.openEmail ? desktop.openEmail.body : ""
                                              color: "${c "base05"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 10
                                              wrapMode: Text.WordWrap
                                              textFormat: Text.PlainText
                                              clip: true
                                          }
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
                                      text: desktop.emails.length + " messages · Connected to Exchange Server"
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
                          height: 400
                          anchors.verticalCenter: parent.verticalCenter
                          anchors.verticalCenterOffset: -40
                          x: Math.max(calWindow.x + calWindow.width + 80,
                                      parent.width / 2 + (parent.width / 2 - width) / 2)

                          // ── Translucent dark backing panel ─────────
                          Rectangle {
                              anchors.fill: parent
                              anchors.margins: -20
                              radius: 10
                              color: "${ca "base00" "cc"}"
                              border.width: 1
                              border.color: "${ca "base0D" "55"}"

                              // Subtle inner highlight (Win7 Aero feel)
                              Rectangle {
                                  anchors.fill: parent
                                  anchors.margins: 1
                                  radius: parent.radius - 1
                                  color: "transparent"
                                  border.width: 1
                                  border.color: "${ca "base05" "11"}"
                              }
                          }

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
                              // Only the emails flagged as toast notifications.
                              model: desktop.emails.filter(function(e) { return e.notification })
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
                      // ║  WINDOWS SETUP — UPDATING GNU/WINDOWS  ║
                      // ║  (top-center, modal-style)             ║
                      // ╚════════════════════════════════════════╝
                      Rectangle {
                          id: installerWindow
                          anchors.top: parent.top
                          anchors.horizontalCenter: parent.horizontalCenter
                          anchors.topMargin: 28
                          width: 520
                          height: 196
                          radius: 6
                          color: "${ca "base01" "f2"}"
                          border.width: 1
                          border.color: "${c "base02"}"

                          // Aero glow
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

                                  RowLayout {
                                      anchors.left: parent.left
                                      anchors.leftMargin: 10
                                      anchors.verticalCenter: parent.verticalCenter
                                      spacing: 6

                                      Text {
                                          text: "󰖳"
                                          color: "${c "base0D"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 13
                                      }
                                      Text {
                                          text: "GNU/Windows Update Assistant"
                                          color: "${c "base05"}"
                                          font.family: "Segoe UI"
                                          font.pixelSize: 12
                                      }
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

                              // ── Body ──────────────────────────
                              Rectangle {
                                  Layout.fillWidth: true
                                  Layout.fillHeight: true
                                  color: "${ca "base00" "ee"}"

                                  RowLayout {
                                      anchors.fill: parent
                                      anchors.margins: 18
                                      spacing: 18

                                      // Tux — because what you're calling
                                      // Windows is in fact GNU/Windows.
                                      Rectangle {
                                          Layout.preferredWidth: 64
                                          Layout.preferredHeight: 64
                                          Layout.alignment: Qt.AlignTop
                                          radius: 8
                                          color: "${ca "base0D" "22"}"
                                          border.width: 1
                                          border.color: "${ca "base0D" "66"}"

                                          Text {
                                              anchors.centerIn: parent
                                              text: ""
                                              color: "${c "base0D"}"
                                              font.family: "RobotoMono Nerd Font"
                                              font.pixelSize: 38
                                          }
                                      }

                                      ColumnLayout {
                                          Layout.fillWidth: true
                                          Layout.fillHeight: true
                                          spacing: 6

                                          Text {
                                              text: "Updating GNU/Windows…"
                                              color: "${c "base05"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 16
                                              font.weight: Font.DemiBold
                                          }

                                          Text {
                                              id: installerStep
                                              Layout.fillWidth: true
                                              text: "Step 142 of ∞ — Renaming \"Windows\" to \"GNU/Windows\"…"
                                              color: "${c "base04"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 11
                                              elide: Text.ElideRight

                                              property var steps: [
                                                  "Step 142 of ∞ — Renaming \"Windows\" to \"GNU/Windows\"…",
                                                  "Step 143 of ∞ — Replacing kernel32.dll with GNU Hurd…",
                                                  "Step 144 of ∞ — Asking Stallman for permission…",
                                                  "Step 145 of ∞ — Translating regedit into S-expressions…",
                                                  "Step 146 of ∞ — Rebinding Ctrl-Alt-Delete to M-x crash-and-reboot…",
                                                  "Step 147 of ∞ — Liberating six of Bill's islands…",
                                                  "Step 148 of ∞ — Recompiling explorer.exe under GPLv3…",
                                                  "Step 149 of ∞ — Wrapping cmd.exe inside M-x shell…",
                                                  "Step 150 of ∞ — Negotiating with systemd (unsuccessfully)…"
                                              ]
                                              property int stepIdx: 0

                                              Timer {
                                                  running: true; repeat: true; interval: 2400
                                                  onTriggered: {
                                                      installerStep.stepIdx =
                                                          (installerStep.stepIdx + 1) % installerStep.steps.length
                                                      installerStep.text =
                                                          installerStep.steps[installerStep.stepIdx]
                                                  }
                                              }
                                          }

                                          // Progress bar
                                          Rectangle {
                                              Layout.fillWidth: true
                                              Layout.preferredHeight: 16
                                              Layout.topMargin: 4
                                              radius: 3
                                              color: "${ca "base00" "ee"}"
                                              border.width: 1
                                              border.color: "${c "base02"}"

                                              Rectangle {
                                                  id: progressFill
                                                  anchors.left: parent.left
                                                  anchors.top: parent.top
                                                  anchors.bottom: parent.bottom
                                                  anchors.margins: 2
                                                  radius: 2
                                                  gradient: Gradient {
                                                      orientation: Gradient.Horizontal
                                                      GradientStop { position: 0.0; color: "${c "base0B"}" }
                                                      GradientStop { position: 1.0; color: "${c "base0D"}" }
                                                  }

                                                  property real pct: 0.62
                                                  width: (parent.width - 4) * pct

                                                  SequentialAnimation on pct {
                                                      loops: Animation.Infinite
                                                      NumberAnimation { from: 0.04; to: 0.97; duration: 9000; easing.type: Easing.InOutCubic }
                                                      NumberAnimation { from: 0.97; to: 0.10; duration: 600;  easing.type: Easing.OutQuad }
                                                  }
                                              }
                                          }

                                          Text {
                                              text: "Estimated time remaining: 4 freedoms"
                                              color: "${c "base04"}"
                                              font.family: "Segoe UI"
                                              font.pixelSize: 10
                                              font.italic: true
                                          }

                                          Item { Layout.fillHeight: true }
                                      }
                                  }
                              }
                          }
                      }

                      // ╔════════════════════════════════════════╗
                      // ║  TASKBAR (bottom — Win11-style, matches Bar.qml) ║
                      // ╚════════════════════════════════════════╝
                      Rectangle {
                          id: taskbar
                          anchors.left: parent.left
                          anchors.right: parent.right
                          anchors.bottom: parent.bottom
                          height: 48
                          // Win11-style: slightly translucent strip, no rounding
                          color: "${ca "base00" "b8"}"
                          border.width: 0

                          // ── Top border line (Win11 thin separator) ───
                          Rectangle {
                              anchors.top: parent.top
                              anchors.left: parent.left
                              anchors.right: parent.right
                              height: 1
                              color: "${ca "base02" "88"}"
                          }

                          // ══════════════════════════════════════════
                          // CENTER ZONE — NixOS logo + dock
                          // ══════════════════════════════════════════
                          Row {
                              id: taskbarCenter
                              anchors.horizontalCenter: parent.horizontalCenter
                              anchors.verticalCenter: parent.verticalCenter
                              spacing: 2

                              // ── NixOS / launcher button ──────────
                              Item {
                                  id: tbNixBtn
                                  width: 42
                                  height: 42

                                  Rectangle {
                                      anchors.fill: parent
                                      radius: 8
                                      color: "transparent"

                                      Text {
                                          anchors.centerIn: parent
                                          text: "󱄅"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 26
                                          color: "${ca "base0D" "cc"}"
                                      }
                                  }
                              }

                              // ── Thin separator ───────────────────
                              Rectangle {
                                  width: 1
                                  height: 28
                                  anchors.verticalCenter: parent.verticalCenter
                                  color: "${ca "base02" "88"}"
                              }

                              // ── Workspace dock (Win11-style, static) ─
                              Row {
                                  id: tbDockRow
                                  anchors.verticalCenter: parent.verticalCenter
                                  spacing: 2

                                  Repeater {
                                      model: [
                                          { wsId: 1, icon: "󰈹", label: "Browser",         active: false, hasWindows: true  },
                                          { wsId: 2, icon: "󰅴", label: "Coding",          active: true,  hasWindows: true  },
                                          { wsId: 3, icon: "󰆍", label: "Terminal",        active: false, hasWindows: true  },
                                          { wsId: 5, icon: "󰢹", label: "Virtual Machine", active: false, hasWindows: false },
                                          { wsId: 8, icon: "󰒱", label: "Slack",           active: false, hasWindows: true  },
                                          { wsId: 9, icon: "󰙯", label: "Discord",         active: false, hasWindows: false }
                                      ]

                                      Item {
                                          id: tbDockItem
                                          required property var modelData
                                          width: 44
                                          height: 44

                                          Rectangle {
                                              anchors.centerIn: parent
                                              width: tbDockItem.modelData.active ? 38 : 34
                                              height: tbDockItem.modelData.active ? 38 : 34
                                              radius: 8
                                              color: tbDockItem.modelData.active
                                                  ? "${ca "base0D" "33"}"
                                                  : "transparent"
                                              border.width: tbDockItem.modelData.active ? 1 : 0
                                              border.color: "${ca "base0D" "88"}"

                                              Text {
                                                  anchors.centerIn: parent
                                                  text: tbDockItem.modelData.icon
                                                  font.family: "RobotoMono Nerd Font"
                                                  font.pixelSize: 20
                                                  color: tbDockItem.modelData.active
                                                      ? "${c "base0D"}"
                                                      : (tbDockItem.modelData.hasWindows
                                                          ? "${c "base05"}"
                                                          : "${c "base04"}")
                                              }
                                          }

                                          // Win11-style running/active indicator (underline)
                                          Rectangle {
                                              anchors.horizontalCenter: parent.horizontalCenter
                                              anchors.bottom: parent.bottom
                                              anchors.bottomMargin: 2
                                              width: tbDockItem.modelData.active
                                                  ? 14
                                                  : (tbDockItem.modelData.hasWindows ? 4 : 0)
                                              height: 3
                                              radius: 1.5
                                              color: "${c "base0D"}"
                                              visible: tbDockItem.modelData.active || tbDockItem.modelData.hasWindows
                                              opacity: tbDockItem.modelData.active ? 1.0 : 0.75
                                          }
                                      }
                                  }

                                  // ── Thin separator ───────────────────
                                  Rectangle {
                                      width: 1
                                      height: 28
                                      anchors.verticalCenter: parent.verticalCenter
                                      color: "${ca "base02" "88"}"
                                  }

                                  // ── Calendar taskbar button ───────────
                                  Item {
                                      id: tbCalBtn
                                      width: 44
                                      height: 44

                                      Rectangle {
                                          anchors.centerIn: parent
                                          width: !desktop.calMinimized ? 38 : 34
                                          height: !desktop.calMinimized ? 38 : 34
                                          radius: 8
                                          color: !desktop.calMinimized
                                              ? "${ca "base0D" "33"}"
                                              : (calBtnHover.hovered ? "${ca "base02" "66"}" : "transparent")
                                          border.width: !desktop.calMinimized ? 1 : 0
                                          border.color: "${ca "base0D" "88"}"
                                          Behavior on color { ColorAnimation { duration: 120 } }

                                          Text {
                                              anchors.centerIn: parent
                                              text: "󰃭"
                                              font.family: "RobotoMono Nerd Font"
                                              font.pixelSize: 20
                                              color: !desktop.calMinimized
                                                  ? "${c "base0D"}"
                                                  : "${c "base05"}"
                                          }

                                          HoverHandler { id: calBtnHover }
                                          MouseArea {
                                              anchors.fill: parent
                                              cursorShape: Qt.PointingHandCursor
                                              onClicked: desktop.calMinimized = !desktop.calMinimized
                                          }
                                      }

                                      Rectangle {
                                          anchors.horizontalCenter: parent.horizontalCenter
                                          anchors.bottom: parent.bottom
                                          anchors.bottomMargin: 2
                                          width: !desktop.calMinimized ? 14 : 4
                                          height: 3
                                          radius: 1.5
                                          color: "${c "base0D"}"
                                          opacity: !desktop.calMinimized ? 1.0 : 0.6
                                          Behavior on width { NumberAnimation { duration: 150 } }
                                      }
                                  }

                                  // ── Outlook taskbar button ────────────
                                  Item {
                                      id: tbOutBtn
                                      width: 44
                                      height: 44

                                      Rectangle {
                                          anchors.centerIn: parent
                                          width: !desktop.outlookMinimized ? 38 : 34
                                          height: !desktop.outlookMinimized ? 38 : 34
                                          radius: 8
                                          color: !desktop.outlookMinimized
                                              ? "${ca "base0D" "33"}"
                                              : (outBtnHover.hovered ? "${ca "base02" "66"}" : "transparent")
                                          border.width: !desktop.outlookMinimized ? 1 : 0
                                          border.color: "${ca "base0D" "88"}"
                                          Behavior on color { ColorAnimation { duration: 120 } }

                                          Text {
                                              anchors.centerIn: parent
                                              text: "󰇮"
                                              font.family: "RobotoMono Nerd Font"
                                              font.pixelSize: 20
                                              color: !desktop.outlookMinimized
                                                  ? "${c "base0D"}"
                                                  : "${c "base05"}"
                                          }

                                          HoverHandler { id: outBtnHover }
                                          MouseArea {
                                              anchors.fill: parent
                                              cursorShape: Qt.PointingHandCursor
                                              onClicked: desktop.outlookMinimized = !desktop.outlookMinimized
                                          }
                                      }

                                      Rectangle {
                                          anchors.horizontalCenter: parent.horizontalCenter
                                          anchors.bottom: parent.bottom
                                          anchors.bottomMargin: 2
                                          width: !desktop.outlookMinimized ? 14 : 4
                                          height: 3
                                          radius: 1.5
                                          color: "${c "base0D"}"
                                          opacity: !desktop.outlookMinimized ? 1.0 : 0.6
                                          Behavior on width { NumberAnimation { duration: 150 } }
                                      }
                                  }
                              }
                          }

                          // ══════════════════════════════════════════
                          // RIGHT ZONE — tray corner + clock
                          // ══════════════════════════════════════════
                          Row {
                              id: taskbarRight
                              anchors.right: parent.right
                              anchors.verticalCenter: parent.verticalCenter
                              anchors.rightMargin: 6
                              spacing: 2

                              // ── Win11-style system tray corner (static 2x2 grid) ─
                              Item {
                                  id: tbTrayCorner
                                  width: 44
                                  height: 42

                                  Rectangle {
                                      anchors.fill: parent
                                      radius: 6
                                      color: "transparent"

                                      Grid {
                                          anchors.centerIn: parent
                                          columns: 2
                                          rows: 2
                                          spacing: 2

                                          Repeater {
                                              model: [
                                                  { icon: "󰆩", color: "${c "base05"}" },
                                                  { icon: "󰕾", color: "${c "base05"}" },
                                                  { icon: "󰂄", color: "${c "base0B"}" },
                                                  { icon: "󰂯", color: "${c "base0D"}" }
                                              ]
                                              Item {
                                                  required property var modelData
                                                  width: 14
                                                  height: 14
                                                  Text {
                                                      anchors.centerIn: parent
                                                      text: parent.modelData.icon
                                                      color: parent.modelData.color
                                                      font.family: "RobotoMono Nerd Font"
                                                      font.pixelSize: 11
                                                  }
                                              }
                                          }
                                      }
                                  }
                              }

                              // ── Thin separator ───────────────────
                              Rectangle {
                                  width: 1
                                  height: 26
                                  anchors.verticalCenter: parent.verticalCenter
                                  color: "${ca "base02" "66"}"
                              }

                              // ── Clock block (two-line) ───────────
                              Item {
                                  id: tbClockItem
                                  width: tbClockCol.implicitWidth + 20
                                  height: 42

                                  Rectangle {
                                      anchors.fill: parent
                                      radius: 6
                                      color: "transparent"

                                      Column {
                                          id: tbClockCol
                                          anchors.centerIn: parent
                                          spacing: 0

                                          Text {
                                              id: tbClockTime
                                              anchors.horizontalCenter: parent.horizontalCenter
                                              text: Qt.formatDateTime(new Date(), "hh:mm AP")
                                              font.family: "RobotoMono Nerd Font"
                                              font.pixelSize: 14
                                              font.weight: Font.Medium
                                              color: "${c "base05"}"
                                              Timer {
                                                  running: true; repeat: true; interval: 1000; triggeredOnStart: true
                                                  onTriggered: tbClockTime.text =
                                                      Qt.formatDateTime(new Date(), "hh:mm AP")
                                              }
                                          }
                                          Text {
                                              id: tbClockDate
                                              anchors.horizontalCenter: parent.horizontalCenter
                                              text: Qt.formatDateTime(new Date(), "dd/MM/yyyy")
                                              font.family: "RobotoMono Nerd Font"
                                              font.pixelSize: 10
                                              color: "${c "base04"}"
                                              Timer {
                                                  running: true; repeat: true; interval: 60000; triggeredOnStart: true
                                                  onTriggered: tbClockDate.text =
                                                      Qt.formatDateTime(new Date(), "dd/MM/yyyy")
                                              }
                                          }
                                      }
                                  }
                              }

                              // ── Show-desktop strip (Win11 rightmost edge) ─
                              Rectangle {
                                  width: 4
                                  height: 42
                                  radius: 2
                                  color: "transparent"
                                  anchors.verticalCenter: parent.verticalCenter
                              }
                          }
                      }
                  }
              }
          }
      }
  }
''
