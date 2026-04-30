{ c, ca }:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io
  import Quickshell.Services.Pipewire

  Scope {
      id: root

      property bool opened: false
      function show()   { root.opened = true }
      function hide()   { root.opened = false }
      function toggle() { root.opened = !root.opened }

      IpcHandler {
          target: "volume"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      PwObjectTracker {
          objects: {
              var arr = []
              if (Pipewire.defaultAudioSink)   arr.push(Pipewire.defaultAudioSink)
              if (Pipewire.defaultAudioSource) arr.push(Pipewire.defaultAudioSource)
              return arr
          }
      }

      readonly property var sink:   Pipewire.defaultAudioSink
      readonly property var source: Pipewire.defaultAudioSource

      PanelWindow {
          id: panel
          visible: root.opened || card.opacity > 0.01

          WlrLayershell.namespace: "quickshell-volume"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors { top: true; right: true }
          margins { top: 50; right: 8 }
          implicitWidth: 360
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

              transformOrigin: Item.Top
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
                  spacing: 12

                  Text {
                      text: "󰕾  Audio"
                      color: "${c "base0E"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 14
                      font.weight: Font.Medium
                  }

                  // ── Output (sink) ────────────────────────────
                  ColumnLayout {
                      Layout.fillWidth: true
                      spacing: 6

                      RowLayout {
                          Layout.fillWidth: true
                          spacing: 10

                          Rectangle {
                              Layout.preferredWidth: 30
                              Layout.preferredHeight: 30
                              radius: 15
                              color: outIconHover.hovered
                                  ? "${ca "base02" "cc"}"
                                  : "transparent"
                              Behavior on color { ColorAnimation { duration: 120 } }
                              HoverHandler { id: outIconHover }
                              Text {
                                  anchors.centerIn: parent
                                  text: (root.sink && root.sink.audio && root.sink.audio.muted)
                                      ? "󰝟" : "󰕾"
                                  color: (root.sink && root.sink.audio && root.sink.audio.muted)
                                      ? "${c "base08"}" : "${c "base0E"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 18
                              }
                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: {
                                      if (root.sink && root.sink.audio)
                                          root.sink.audio.muted = !root.sink.audio.muted
                                  }
                              }
                          }

                          ColumnLayout {
                              Layout.fillWidth: true
                              spacing: 0
                              Text {
                                  text: "Output"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              Text {
                                  Layout.fillWidth: true
                                  text: root.sink
                                      ? (root.sink.description || root.sink.name || "—")
                                      : "No device"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                                  elide: Text.ElideRight
                              }
                          }

                          Text {
                              text: root.sink && root.sink.audio
                                  ? Math.round(root.sink.audio.volume * 100) + "%"
                                  : "—"
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                          }
                      }

                      Slider {
                          id: outSlider
                          Layout.fillWidth: true
                          from: 0; to: 1
                          value: root.sink && root.sink.audio
                              ? root.sink.audio.volume : 0
                          onMoved: {
                              if (root.sink && root.sink.audio)
                                  root.sink.audio.volume = value
                          }
                      }
                  }

                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 1
                      color: "${ca "base02" "80"}"
                      visible: root.source !== null
                  }

                  // ── Input (source) ───────────────────────────
                  ColumnLayout {
                      visible: root.source !== null
                      Layout.fillWidth: true
                      spacing: 6

                      RowLayout {
                          Layout.fillWidth: true
                          spacing: 10

                          Rectangle {
                              Layout.preferredWidth: 30
                              Layout.preferredHeight: 30
                              radius: 15
                              color: inIconHover.hovered
                                  ? "${ca "base02" "cc"}"
                                  : "transparent"
                              Behavior on color { ColorAnimation { duration: 120 } }
                              HoverHandler { id: inIconHover }
                              Text {
                                  anchors.centerIn: parent
                                  text: (root.source && root.source.audio && root.source.audio.muted)
                                      ? "󰍭" : "󰍬"
                                  color: (root.source && root.source.audio && root.source.audio.muted)
                                      ? "${c "base08"}" : "${c "base0B"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 18
                              }
                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: {
                                      if (root.source && root.source.audio)
                                          root.source.audio.muted = !root.source.audio.muted
                                  }
                              }
                          }

                          ColumnLayout {
                              Layout.fillWidth: true
                              spacing: 0
                              Text {
                                  text: "Input"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              Text {
                                  Layout.fillWidth: true
                                  text: root.source
                                      ? (root.source.description || root.source.name || "—")
                                      : "No device"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                                  elide: Text.ElideRight
                              }
                          }

                          Text {
                              text: root.source && root.source.audio
                                  ? Math.round(root.source.audio.volume * 100) + "%"
                                  : "—"
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                          }
                      }

                      Slider {
                          Layout.fillWidth: true
                          from: 0; to: 1
                          value: root.source && root.source.audio
                              ? root.source.audio.volume : 0
                          onMoved: {
                              if (root.source && root.source.audio)
                                  root.source.audio.volume = value
                          }
                      }
                  }
              }
          }
      }
  }
''
