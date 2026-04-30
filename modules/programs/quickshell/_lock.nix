{ c, ca }:
''
  import QtQuick
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io
  import Quickshell.Services.Pam

  Scope {
      id: root

      function lock()   { sessionLock.locked = true }
      function unlock() { sessionLock.locked = false }

      IpcHandler {
          target: "lock"
          function lock()   { root.lock() }
          function unlock() { root.unlock() }
      }

      WlSessionLock {
          id: sessionLock
          locked: false

          WlSessionLockSurface {
              id: surface
              color: "${c "base00"}"

              property bool authFailed: false
              property bool authenticating: false

              Rectangle {
                  anchors.fill: parent
                  color: "${c "base00"}"

                  // ── Time / date ──────────────────────────────
                  Column {
                      anchors.centerIn: parent
                      spacing: 24

                      Text {
                          id: lockTime
                          anchors.horizontalCenter: parent.horizontalCenter
                          text: ""
                          color: "${c "base05"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 96
                          font.weight: Font.Light
                          Timer {
                              running: true; repeat: true; interval: 1000; triggeredOnStart: true
                              onTriggered: lockTime.text =
                                  Qt.formatDateTime(new Date(), "HH:mm")
                          }
                      }

                      Text {
                          id: lockDate
                          anchors.horizontalCenter: parent.horizontalCenter
                          text: ""
                          color: "${c "base0D"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 18
                          Timer {
                              running: true; repeat: true; interval: 30000; triggeredOnStart: true
                              onTriggered: lockDate.text =
                                  Qt.formatDateTime(new Date(), "dddd, MMMM d")
                          }
                      }

                      // ── Password input ───────────────────────
                      Rectangle {
                          id: pwBox
                          anchors.horizontalCenter: parent.horizontalCenter
                          width: 340
                          height: 50
                          radius: 25
                          color: "${ca "base01" "cc"}"
                          border.width: 1
                          border.color: surface.authFailed
                              ? "${c "base08"}"
                              : surface.authenticating
                                  ? "${c "base0A"}"
                                  : "${c "base02"}"

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
                              anchors.leftMargin: 18
                              anchors.rightMargin: 18
                              verticalAlignment: TextInput.AlignVCenter
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 14
                              echoMode: TextInput.Password
                              enabled: !surface.authenticating
                              focus: true

                              Keys.onEnterPressed: surface.tryAuth()
                              Keys.onReturnPressed: surface.tryAuth()
                          }

                          Text {
                              visible: passwordInput.text === "" && !surface.authenticating
                              anchors.left: parent.left
                              anchors.leftMargin: 18
                              anchors.verticalCenter: parent.verticalCenter
                              text: "Enter password"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 14
                          }
                      }

                      Text {
                          anchors.horizontalCenter: parent.horizontalCenter
                          visible: surface.authFailed
                          text: "Authentication failed"
                          color: "${c "base08"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 12
                      }
                  }
              }

              function tryAuth() {
                  if (authenticating) return
                  if (passwordInput.text === "") return
                  authFailed = false
                  authenticating = true
                  pam.start()
              }

              PamContext {
                  id: pam
                  config: "quickshell"

                  onPamMessage: function(msg) {
                      respond(passwordInput.text)
                  }
                  onCompleted: function(result) {
                      surface.authenticating = false
                      if (result === PamResult.Success) {
                          passwordInput.text = ""
                          surface.authFailed = false
                          root.unlock()
                      } else {
                          surface.authFailed = true
                          passwordInput.text = ""
                          shakeAnim.start()
                      }
                  }
              }
          }
      }
  }
''
