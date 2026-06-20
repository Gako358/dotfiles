{
  c,
  ca,
  lib,
  user ? "merrinx",
  sessionCommand,
  greeterMonitors ? [ ],
}:
let
  cmdJson = "[" + (lib.concatMapStringsSep ", " (s: ''"${s}"'') sessionCommand) + "]";
  monitorsJs =
    if greeterMonitors == [ ] then
      "[]"
    else
      "[" + (lib.concatMapStringsSep ", " (m: ''"${m}"'') greeterMonitors) + "]";
in
''
  import QtQuick
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Services.Greetd

  ShellRoot {
      id: root

      // ── Auth state ───────────────────────────────────
      property string user: "${user}"
      property string password: ""
      property bool   authFailed: false
      property bool   authenticating: false
      property string statusMsg: ""

      readonly property var sessionCommand: ${cmdJson}

      signal shakeRequested()

      property var greeterMonitors: ${monitorsJs}

      readonly property var targetScreen: {
          var ms = root.greeterMonitors
          var screens = Quickshell.screens
          if (!ms || ms.length === 0) return null
          if (!screens || screens.length === 0) return null
          for (var i = 0; i < ms.length; ++i) {
              var ident = ms[i]
              if (!ident) continue
              if (ident.indexOf("desc:") === 0) ident = ident.substring(5)
              for (var j = 0; j < screens.length; ++j) {
                  var s = screens[j]
                  var desc = (s.name || "") + " "
                           + (s.model || "") + " "
                           + (s.serialNumber || "")
                  if (desc.indexOf(ident) >= 0) return s
              }
          }
          return null
      }

      readonly property var screenBounds: {
          var screens = Quickshell.screens
          if (!screens || screens.length === 0)
              return { "x": 0, "y": 0, "width": 1920, "height": 1080 }
          var minX = screens[0].x, minY = screens[0].y
          var maxX = screens[0].x + screens[0].width
          var maxY = screens[0].y + screens[0].height
          for (var i = 1; i < screens.length; ++i) {
              var s = screens[i]
              if (s.x < minX) minX = s.x
              if (s.y < minY) minY = s.y
              if (s.x + s.width  > maxX) maxX = s.x + s.width
              if (s.y + s.height > maxY) maxY = s.y + s.height
          }
          return { "x": minX, "y": minY, "width": maxX - minX, "height": maxY - minY }
      }

      function startAuth() {
          if (root.authenticating) return
          if (root.password === "") return
          root.authFailed = false
          root.statusMsg = ""
          root.authenticating = true
          Greetd.createSession(root.user)
      }

      // ── greetd protocol ──────────────────────────────
      Connections {
          target: Greetd

          function onAuthMessage(message, error, responseRequired, echoResponse) {
              if (responseRequired) {
                  Greetd.respond(root.password)
              } else if (error) {
                  root.statusMsg = message
              }
          }

          function onAuthFailure(message) {
              root.authenticating = false
              root.authFailed = true
              root.password = ""
              root.statusMsg = message
              root.shakeRequested()
          }

          function onError(message) {
              root.authenticating = false
              root.authFailed = true
              root.statusMsg = message
              root.shakeRequested()
          }

          function onReadyToLaunch() {
              // greetd wants the greeter gone ASAP — no animations here.
              Greetd.launch(root.sessionCommand)
          }
      }

      FloatingWindow {
          id: win
          implicitWidth: root.screenBounds.width
          implicitHeight: root.screenBounds.height
          color: "${c "base00"}"

          Rectangle {
              anchors.fill: parent
              color: "${c "base00"}"
          }

          Item {
          id: stage
          x: root.targetScreen ? root.targetScreen.x - root.screenBounds.x : 0
          y: root.targetScreen ? root.targetScreen.y - root.screenBounds.y : 0
          width:  root.targetScreen ? root.targetScreen.width  : parent.width
          height: root.targetScreen ? root.targetScreen.height : parent.height

          // ── Darkish background wash ──────────────────
          Rectangle {
              anchors.fill: parent
              gradient: Gradient {
                  GradientStop { position: 0.0; color: "${c "base00"}" }
                  GradientStop { position: 0.55; color: "${ca "base01" "cc"}" }
                  GradientStop { position: 1.0; color: "${c "base00"}" }
              }
          }

          Rectangle {
              anchors.fill: parent
              color: "transparent"
              border.width: 140
              border.color: "${ca "base00" "55"}"
          }

          // ── Clock (top-centre) ───────────────────────
          ColumnLayout {
              anchors.top: parent.top
              anchors.topMargin: 56
              anchors.horizontalCenter: parent.horizontalCenter
              spacing: 0

              Text {
                  id: clock
                  Layout.alignment: Qt.AlignHCenter
                  text: ""
                  color: "${c "base05"}"
                  font.family: "Segoe UI"
                  font.pixelSize: 64
                  font.weight: Font.Light
                  Timer {
                      running: true; repeat: true; interval: 1000; triggeredOnStart: true
                      onTriggered: clock.text = Qt.formatDateTime(new Date(), "HH:mm")
                  }
              }
              Text {
                  id: dateLabel
                  Layout.alignment: Qt.AlignHCenter
                  text: ""
                  color: "${c "base04"}"
                  font.family: "Segoe UI"
                  font.pixelSize: 16
                  Timer {
                      running: true; repeat: true; interval: 30000; triggeredOnStart: true
                      onTriggered: dateLabel.text =
                          Qt.formatDateTime(new Date(), "dddd, MMMM d")
                  }
              }
          }

          // ── Centre row: Tip card  +  Login tile ──────
          RowLayout {
              anchors.centerIn: parent
              spacing: 48

              // ╔══════════════════════════════════════╗
              // ║  TIP OF THE DAY (random each load)   ║
              // ╚══════════════════════════════════════╝
              Rectangle {
                  id: tipCard
                  Layout.preferredWidth: 400
                  Layout.preferredHeight: 340
                  Layout.alignment: Qt.AlignVCenter
                  radius: 12
                  color: "${ca "base01" "cc"}"
                  border.width: 1
                  border.color: "${ca "base0D" "55"}"

                  property var tips: [
                      { tag: "WoT × SCRUM",  text: "The Wheel of Time has no beginning, but your sprint ends Friday at 17:00, and the Dark One does not accept story-point inflation." },
                      { tag: "EMACS × WoT",  text: "M-x doomsday: Rand al'Thor reached Tarmon Gai'don in fewer keystrokes than it takes a Vim user to quit." },
                      { tag: "RMS × WoT",    text: "I'd just like to interject: what you call 'the One Power' is in fact GNU/saidin, channeled exclusively under the GPLv3." },
                      { tag: "SCRUM × WoT",  text: "Aes Sedai never lie. They merely re-estimate the truth during backlog refinement, bound by the Oath Rod of the Definition of Done." },
                      { tag: "EMACS",        text: "Emacs is a great operating system, lacking only a decent Wheel of Time adaptation and a daily standup that ends on time." },
                      { tag: "TRUMP × WoT",  text: "Trump on being ta'veren: 'Nobody bends the Pattern like me. The Wheel weaves, but believe me, it weaves BIGLY in my favour.'" },
                      { tag: "SCRUM",        text: "A retrospective is the Forsaken's favourite ceremony: everyone swears 'this sprint we change nothing' and means it." },
                      { tag: "EMACS × WoT",  text: "C-x C-c does not work in Tel'aran'rhiod. You must simply will the buffer to close. Stallman wills harder than you." },
                      { tag: "SCRUM × WoT",  text: "The Dragon Reborn estimated Tarmon Gai'don at 3 story points. It took an entire Age. A classic under-estimation." },
                      { tag: "RMS × SCRUM",  text: "Stallman renamed the Three Oaths to the Four Freedoms and now forbids any Aes Sedai from speaking proprietary words." },
                      { tag: "SCRUM × WoT",  text: "White Tower standup: 'Yesterday I balefired prod. Today I balefire it harder. Blockers: the Pattern itself.'" },
                      { tag: "TRUMP",        text: "Trump: 'I know the Forsaken, great guys, terrific people, and they all say my burndown chart is the most beautiful chart. Tremendous.'" },
                      { tag: "EMACS",        text: "Emacs ships with a psychotherapist (M-x doctor) because no Scrum Master has ever survived sprint planning without one." },
                      { tag: "WoT × SCRUM",  text: "Min reads the auras of the team and sees only one viewing above the Product Owner: a Gantt chart, and it is on fire." },
                      { tag: "RMS × WoT",    text: "GNU Mat Cauthon will gamble his copyleft on a single dice roll, but he will never, ever sign a proprietary EULA." },
                      { tag: "SCRUM",        text: "Velocity is like the True Source: men who draw too much of it go mad, usually right around Sprint 14." },
                      { tag: "RMS",          text: "Stallman insists it is not the 'Last Battle'. It is the 'GNU/Last Battle, with Linux'." },
                      { tag: "WoT × SCRUM",  text: "Lan's definition of done: the enemy is dead, the branch is merged, and there are zero open tickets left in Malkier." },
                      { tag: "TRUMP × SCRUM",text: "Trump will build a great wall around the backlog, the most beautiful wall, and the stakeholders are going to pay for it." },
                      { tag: "WoT × SCRUM",  text: "Nynaeve tugs her braid every single time someone says 'we'll just pick it up in the next sprint'." },
                      { tag: "WoT × JIRA",   text: "The Wheel weaves as the Wheel wills, and JIRA closes tickets as JIRA wills — neither one ever consults you first." },
                      { tag: "WoT × SCRUM",  text: "Padan Fain is technical debt: ignored for three books, then suddenly your only blocker at Tarmon Gai'don." },
                      { tag: "RMS",          text: "Stallman's Oath Rod: 'I will not say Open Source, I will not run non-free firmware, and I will not touch a mouse.'" },
                      { tag: "TRUMP × SCRUM",text: "Trump's retro action item: 'We're going to make the standup great again — four hours, standing, very powerful, people are crying.'" },
                      { tag: "WoT × EMACS",  text: "Loial the Ogier writes the most thorough commit messages in the Pattern; each one runs 40 pages and politely cites its grandfathers." }
                  ]
                  property int tipIndex: 0

                  function reroll() {
                      if (tipCard.tips.length <= 1) return
                      var n = tipCard.tipIndex
                      while (n === tipCard.tipIndex)
                          n = Math.floor(Math.random() * tipCard.tips.length)
                      tipCard.tipIndex = n
                  }

                  Component.onCompleted: tipCard.tipIndex =
                      Math.floor(Math.random() * tipCard.tips.length)

                  ColumnLayout {
                      anchors.fill: parent
                      anchors.margins: 22
                      spacing: 14

                      RowLayout {
                          Layout.fillWidth: true
                          spacing: 10
                          Text {
                              text: "󰌶"
                              color: "${c "base0A"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 22
                          }
                          Text {
                              Layout.fillWidth: true
                              text: "Tip of the Day"
                              color: "${c "base0D"}"
                              font.family: "Segoe UI"
                              font.pixelSize: 18
                              font.weight: Font.DemiBold
                          }
                          Rectangle {
                              Layout.preferredHeight: 18
                              Layout.preferredWidth: tagText.implicitWidth + 14
                              radius: 4
                              color: "${ca "base0E" "33"}"
                              border.width: 1
                              border.color: "${ca "base0E" "66"}"
                              Text {
                                  id: tagText
                                  anchors.centerIn: parent
                                  text: tipCard.tips[tipCard.tipIndex].tag
                                  color: "${c "base0E"}"
                                  font.family: "Segoe UI"
                                  font.pixelSize: 9
                                  font.weight: Font.Bold
                              }
                          }
                      }

                      Rectangle {
                          Layout.fillWidth: true
                          Layout.preferredHeight: 1
                          color: "${c "base02"}"
                      }

                      Text {
                          Layout.fillWidth: true
                          Layout.fillHeight: true
                          text: tipCard.tips[tipCard.tipIndex].text
                          color: "${c "base05"}"
                          font.family: "Segoe UI"
                          font.pixelSize: 15
                          wrapMode: Text.WordWrap
                          lineHeight: 1.25
                          verticalAlignment: Text.AlignVCenter
                      }

                      Text {
                          Layout.alignment: Qt.AlignRight
                          text: "↻  another one"
                          color: rerollHover.hovered ? "${c "base0D"}" : "${c "base04"}"
                          font.family: "Segoe UI"
                          font.pixelSize: 11
                          font.italic: true
                          Behavior on color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: rerollHover }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: tipCard.reroll()
                          }
                      }
                  }
              }

              // ╔══════════════════════════════════════╗
              // ║  LOGIN TILE                          ║
              // ╚══════════════════════════════════════╝
              Item {
                  id: loginTile
                  Layout.preferredWidth: 360
                  Layout.preferredHeight: 400
                  Layout.alignment: Qt.AlignVCenter

                  Rectangle {
                      anchors.fill: parent
                      anchors.margins: -20
                      radius: 12
                      color: "${ca "base00" "cc"}"
                      border.width: 1
                      border.color: "${ca "base0D" "55"}"

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

                      // Avatar
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

                      // Password field
                      Rectangle {
                          id: pwBox
                          Layout.alignment: Qt.AlignHCenter
                          Layout.preferredWidth: 280
                          Layout.preferredHeight: 32
                          radius: 2
                          color: "${ca "base00" "f2"}"
                          border.width: 1
                          border.color: root.authFailed
                              ? "${c "base08"}"
                              : root.authenticating
                                  ? "${c "base0A"}"
                                  : "${c "base0D"}"

                          Behavior on border.color { ColorAnimation { duration: 200 } }

                          SequentialAnimation on x {
                              id: shakeAnim
                              running: false
                              loops: 1
                              NumberAnimation { from: 0;   to: -10; duration: 50 }
                              NumberAnimation { from: -10; to:  10; duration: 50 }
                              NumberAnimation { from:  10; to: -8;  duration: 50 }
                              NumberAnimation { from: -8;  to:  8;  duration: 50 }
                              NumberAnimation { from:  8;  to:  0;  duration: 50 }
                          }

                          Connections {
                              target: root
                              function onShakeRequested() { shakeAnim.start() }
                          }

                          TextInput {
                              id: passwordInput
                              anchors.fill: parent
                              anchors.leftMargin: 10
                              anchors.rightMargin: 38
                              verticalAlignment: TextInput.AlignVCenter
                              color: "${c "base05"}"
                              font.family: "Segoe UI"
                              font.pixelSize: 13
                              echoMode: TextInput.Password
                              enabled: !root.authenticating
                              focus: true
                              Component.onCompleted: passwordInput.forceActiveFocus()

                              text: root.password
                              onTextEdited: root.password = text

                              Keys.onEnterPressed: root.startAuth()
                              Keys.onReturnPressed: root.startAuth()
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
                              width: 30
                              radius: 2
                              color: passwordInput.text === ""
                                  ? "${ca "base02" "99"}"
                                  : "${c "base0D"}"
                              Behavior on color { ColorAnimation { duration: 150 } }

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
                                  onClicked: root.startAuth()
                              }
                          }
                      }

                      Text {
                          Layout.alignment: Qt.AlignHCenter
                          Layout.preferredWidth: 280
                          horizontalAlignment: Text.AlignHCenter
                          visible: root.authFailed
                          text: root.statusMsg !== ""
                              ? root.statusMsg
                              : "The user name or password is incorrect."
                          color: "${c "base08"}"
                          font.family: "Segoe UI"
                          font.pixelSize: 11
                          wrapMode: Text.WordWrap
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
          }

          // ── Power controls (bottom-right) ────────────
          RowLayout {
              anchors.right: parent.right
              anchors.bottom: parent.bottom
              anchors.rightMargin: 24
              anchors.bottomMargin: 20
              spacing: 10

              Repeater {
                  model: [
                      { icon: "󰜉", label: "Reboot",   color: "${c "base09"}", action: "reboot"   },
                      { icon: "󰐥", label: "Shutdown", color: "${c "base08"}", action: "shutdown" }
                  ]

                  Rectangle {
                      required property var modelData
                      Layout.preferredWidth: 44
                      Layout.preferredHeight: 44
                      radius: 10
                      color: powerHover.hovered ? "${ca "base02" "cc"}" : "${ca "base01" "88"}"
                      border.width: 1
                      border.color: "${c "base02"}"
                      Behavior on color { ColorAnimation { duration: 120 } }

                      HoverHandler { id: powerHover }

                      Text {
                          anchors.centerIn: parent
                          text: modelData.icon
                          color: modelData.color
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 20
                      }

                      MouseArea {
                          anchors.fill: parent
                          cursorShape: Qt.PointingHandCursor
                          onClicked: {
                              if (modelData.action === "reboot")
                                  Quickshell.execDetached(["systemctl", "reboot"])
                              else
                                  Quickshell.execDetached(["systemctl", "poweroff"])
                          }
                      }
                  }
              }
          }

          // ── Footer hint (bottom-centre) ──────────────
          Text {
              anchors.horizontalCenter: parent.horizontalCenter
              anchors.bottom: parent.bottom
              anchors.bottomMargin: 22
              text: "GNU/Windows 7 Enterprise · Service Pack ∞ · Sprint 142, day ∞"
              color: "${ca "base04" "cc"}"
              font.family: "Segoe UI"
              font.pixelSize: 11
              font.italic: true
          }
          }
      }
  }
''
