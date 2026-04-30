{ c, ca }:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io
  import Quickshell.Widgets

  Scope {
      id: root

      property bool opened: false

      function show() {
          root.opened = true
          searchField.forceActiveFocus()
          searchField.selectAll()
      }
      function hide() {
          root.opened = false
          searchField.text = ""
      }
      function toggle() {
          if (root.opened) hide(); else show()
      }

      IpcHandler {
          target: "launcher"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      readonly property var allEntries: {
          var out = []
          var apps = DesktopEntries.applications
          if (!apps) return out
          var n = apps.values ? apps.values.length : 0
          for (var i = 0; i < n; ++i) {
              var e = apps.values[i]
              if (!e || e.noDisplay) continue
              out.push(e)
          }
          out.sort(function(a, b) {
              return (a.name || "").toLowerCase()
                  .localeCompare((b.name || "").toLowerCase())
          })
          return out
      }

      function entryMatches(entry, q) {
          if (!q) return true
          q = q.toLowerCase()
          var n = (entry.name || "").toLowerCase()
          var g = (entry.genericName || "").toLowerCase()
          var c = (entry.comment || "").toLowerCase()
          return n.indexOf(q) !== -1
              || g.indexOf(q) !== -1
              || c.indexOf(q) !== -1
      }

      readonly property var filteredEntries: {
          var q = (searchField.text || "").trim()
          if (!q) return allEntries
          return allEntries.filter(function(e) {
              return entryMatches(e, q)
          })
      }

      function launchEntry(entry) {
          if (!entry) return
          try { entry.execute() }
          catch (err) { console.warn("launcher: execute failed", err) }
          root.hide()
      }

      PanelWindow {
          id: panel
          visible: root.opened || launcherCard.opacity > 0.01

          WlrLayershell.namespace: "quickshell-launcher"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors {
              top: true
              left: true
          }
          margins {
              top: 48
              left: 8
          }

          implicitWidth: 460
          implicitHeight: 520
          color: "transparent"

          Shortcut {
              sequences: ["Escape"]
              onActivated: root.hide()
          }

          Rectangle {
              id: launcherCard
              anchors.fill: parent
              color: "${ca "base00" "ee"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"

              property real slideY: root.opened ? 0 : -18
              transform: Translate { y: launcherCard.slideY }

              transformOrigin: Item.TopLeft
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
                  anchors.margins: 14
                  spacing: 10

                  // ── Search field ─────────────────────────────
                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 40
                      color: "${ca "base01" "cc"}"
                      radius: 10
                      border.width: 1
                      border.color: searchField.activeFocus
                          ? "${c "base0D"}"
                          : "${c "base02"}"
                      Behavior on border.color {
                          ColorAnimation { duration: 150 }
                      }

                      RowLayout {
                          anchors.fill: parent
                          anchors.leftMargin: 12
                          anchors.rightMargin: 12
                          spacing: 8

                          Text {
                              text: "󰍉"
                              color: "${c "base0D"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 16
                          }

                          TextField {
                              id: searchField
                              Layout.fillWidth: true
                              background: null
                              placeholderText: "Search applications…"
                              placeholderTextColor: "${c "base04"}"
                              color: "${c "base05"}"
                              selectByMouse: true
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 14

                              Keys.onPressed: function(event) {
                                  if (event.key === Qt.Key_Down) {
                                      appList.incrementCurrentIndex()
                                      event.accepted = true
                                  } else if (event.key === Qt.Key_Up) {
                                      appList.decrementCurrentIndex()
                                      event.accepted = true
                                  } else if (event.key === Qt.Key_Return
                                          || event.key === Qt.Key_Enter) {
                                      var list = root.filteredEntries
                                      var idx = appList.currentIndex
                                      if (idx < 0 || idx >= list.length) idx = 0
                                      root.launchEntry(list[idx])
                                      event.accepted = true
                                  }
                              }
                          }
                      }
                  }

                  // ── Application list ─────────────────────────
                  ListView {
                      id: appList
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      clip: true
                      spacing: 2
                      model: root.filteredEntries
                      currentIndex: 0
                      keyNavigationEnabled: false
                      boundsBehavior: Flickable.StopAtBounds
                      ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

                      delegate: Rectangle {
                          required property int index
                          required property var modelData
                          width: appList.width
                          height: 52
                          radius: 8
                          color: appList.currentIndex === index
                              ? "${ca "base02" "aa"}"
                              : (hover.hovered ? "${ca "base01" "aa"}" : "transparent")

                          HoverHandler { id: hover }

                          RowLayout {
                              anchors.fill: parent
                              anchors.leftMargin: 10
                              anchors.rightMargin: 10
                              spacing: 12

                              IconImage {
                                  Layout.preferredWidth: 32
                                  Layout.preferredHeight: 32
                                  source: modelData
                                      ? (modelData.icon || "application-x-executable")
                                      : ""
                                  implicitSize: 32
                              }

                              ColumnLayout {
                                  Layout.fillWidth: true
                                  spacing: 0
                                  Text {
                                      Layout.fillWidth: true
                                      text: modelData ? (modelData.name || "") : ""
                                      color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 13
                                      font.weight: Font.Medium
                                      elide: Text.ElideRight
                                  }
                                  Text {
                                      Layout.fillWidth: true
                                      visible: text !== ""
                                      text: modelData
                                          ? (modelData.comment
                                              || modelData.genericName
                                              || "")
                                          : ""
                                      color: "${c "base04"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 10
                                      elide: Text.ElideRight
                                  }
                              }
                          }

                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              hoverEnabled: true
                              onEntered: appList.currentIndex = index
                              onClicked: root.launchEntry(modelData)
                          }
                      }
                  }

                  Text {
                      Layout.fillWidth: true
                      visible: root.filteredEntries.length === 0
                      horizontalAlignment: Text.AlignHCenter
                      text: "No matching applications"
                      color: "${c "base04"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 12
                  }
              }
          }
      }
  }
''
