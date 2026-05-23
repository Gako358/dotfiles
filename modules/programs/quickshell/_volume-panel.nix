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
              var nodes = Pipewire.nodes ? Pipewire.nodes.values : []
              for (var i = 0; i < nodes.length; i++) {
                  var n = nodes[i]
                  if (n && n.audio && !n.isStream) arr.push(n)
              }
              return arr
          }
      }

      readonly property var sink:   Pipewire.defaultAudioSink
      readonly property var source: Pipewire.defaultAudioSource

      function _audioDevices(wantSink) {
          var result = []
          var nodes = Pipewire.nodes ? Pipewire.nodes.values : []
          for (var i = 0; i < nodes.length; i++) {
              var n = nodes[i]
              if (n && n.audio && !n.isStream && n.isSink === wantSink)
                  result.push(n)
          }
          return result
      }

      readonly property var sinks:   _audioDevices(true)
      readonly property var sources: _audioDevices(false)

      function _nodeLabel(n) {
          if (!n) return "—"
          return n.description || n.nickname || n.name || "—"
      }

      // ── Themed device picker ─────────────────────────────────
      component DeviceCombo: ComboBox {
          id: combo

          property var devices: []
          property var currentNode: null
          property string emptyText: "No device"
          property string itemIcon: "󰓃" // generic speaker/output by default
          signal pick(var node)

          Layout.fillWidth: true
          Layout.preferredHeight: 30
          font.family: "RobotoMono Nerd Font"
          font.pixelSize: 11

          model: devices
          enabled: devices.length > 0
          currentIndex: {
              if (!currentNode) return -1
              for (var i = 0; i < devices.length; i++)
                  if (devices[i] === currentNode) return i
              return -1
          }
          displayText: currentIndex >= 0
              ? root._nodeLabel(devices[currentIndex])
              : (devices.length === 0 ? emptyText : "—")

          onActivated: (index) => combo.pick(combo.devices[index])

          background: Rectangle {
              radius: 8
              color: combo.popup.visible
                  ? "${ca "base02" "cc"}"
                  : (combo.hovered ? "${ca "base02" "99"}" : "${ca "base01" "cc"}")
              border.width: 1
              border.color: combo.popup.visible
                  ? "${c "base0E"}"
                  : "${ca "base02" "aa"}"
              Behavior on color  { ColorAnimation { duration: 120 } }
              Behavior on border.color { ColorAnimation { duration: 120 } }
          }

          contentItem: RowLayout {
              spacing: 8

              Text {
                  Layout.leftMargin: 10
                  text: combo.itemIcon
                  color: combo.enabled ? "${c "base0E"}" : "${c "base04"}"
                  font.family: "RobotoMono Nerd Font"
                  font.pixelSize: 13
                  verticalAlignment: Text.AlignVCenter
              }
              Text {
                  Layout.fillWidth: true
                  text: combo.displayText
                  font: combo.font
                  color: combo.enabled ? "${c "base05"}" : "${c "base04"}"
                  verticalAlignment: Text.AlignVCenter
                  elide: Text.ElideRight
              }
          }

          indicator: Text {
              x: combo.width - width - 10
              y: (combo.height - height) / 2
              text: "󰅀" // nf-md-chevron_down
              font.family: "RobotoMono Nerd Font"
              font.pixelSize: 14
              color: combo.enabled ? "${c "base04"}" : "${ca "base04" "80"}"
              rotation: combo.popup.visible ? 180 : 0
              Behavior on rotation { NumberAnimation { duration: 160; easing.type: Easing.OutCubic } }
          }

          delegate: ItemDelegate {
              id: del
              required property var modelData
              required property int index
              width: combo.width
              height: 28
              padding: 0

              contentItem: RowLayout {
                  anchors.fill: parent
                  anchors.leftMargin: 8
                  anchors.rightMargin: 8
                  spacing: 8

                  Text {
                      Layout.preferredWidth: 14
                      text: combo.currentIndex === del.index ? "󰄬" : ""
                      color: "${c "base0E"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 12
                      horizontalAlignment: Text.AlignHCenter
                      verticalAlignment: Text.AlignVCenter
                  }
                  Text {
                      Layout.fillWidth: true
                      text: root._nodeLabel(del.modelData)
                      color: combo.currentIndex === del.index
                          ? "${c "base0E"}"
                          : "${c "base05"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 11
                      elide: Text.ElideRight
                      verticalAlignment: Text.AlignVCenter
                  }
              }

              background: Rectangle {
                  radius: 6
                  color: del.highlighted
                      ? "${ca "base02" "cc"}"
                      : (combo.currentIndex === del.index
                          ? "${ca "base02" "55"}"
                          : "transparent")
              }
              highlighted: combo.highlightedIndex === del.index
          }

          popup: Popup {
              y: combo.height + 4
              width: combo.width
              implicitHeight: Math.min(contentItem.implicitHeight + 8, 240)
              padding: 4

              contentItem: ListView {
                  clip: true
                  implicitHeight: contentHeight
                  model: combo.popup.visible ? combo.delegateModel : null
                  currentIndex: combo.highlightedIndex
                  ScrollIndicator.vertical: ScrollIndicator { }
              }

              background: Rectangle {
                  radius: 10
                  color: "${ca "base00" "f2"}"
                  border.width: 1
                  border.color: "${c "base02"}"
              }

              enter: Transition {
                  NumberAnimation { property: "opacity"; from: 0; to: 1; duration: 140; easing.type: Easing.OutCubic }
                  NumberAnimation { property: "scale";   from: 0.96; to: 1; duration: 180; easing.type: Easing.OutQuint }
              }
              exit: Transition {
                  NumberAnimation { property: "opacity"; from: 1; to: 0; duration: 120; easing.type: Easing.InCubic }
              }
          }
      }

      PanelWindow {
          id: panel
          visible: root.opened || card.opacity > 0.01

          WlrLayershell.namespace: "quickshell-volume"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

              anchors { top: true; bottom: true; left: true; right: true }
              color: "transparent"
          
              Shortcut { sequences: ["Escape"]; onActivated: root.hide() }
          
                  // Full-screen dismiss layer — stops at the bar so it never blocks it
                  MouseArea {
                      anchors.fill: parent
                      anchors.bottomMargin: 48
                      onClicked: root.hide()
                  }
          
              Rectangle {
                  id: card
                  width: 360
                  height: contentCol.implicitHeight + 28
                  anchors.bottom: parent.bottom
                  anchors.right: parent.right
                  anchors.bottomMargin: 52
                  anchors.rightMargin: 8
              color: "${ca "base00" "ee"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"

              property real slideY: root.opened ? 0 : 18
              transform: Translate { y: card.slideY }

              transformOrigin: Item.Bottom
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

                              DeviceCombo {
                                  id: sinkCombo
                                  devices: root.sinks
                                  currentNode: root.sink
                                  itemIcon: "󰓃"
                                  onPick: (node) => {
                                      if (node) Pipewire.preferredDefaultAudioSink = node
                                  }
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
                      visible: root.source !== null || root.sources.length > 0
                  }

                  // ── Input (source) ───────────────────────────
                  ColumnLayout {
                      visible: root.source !== null || root.sources.length > 0
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

                              DeviceCombo {
                                  id: sourceCombo
                                  devices: root.sources
                                  currentNode: root.source
                                  itemIcon: "󰍬"
                                  onPick: (node) => {
                                      if (node) Pipewire.preferredDefaultAudioSource = node
                                  }
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
