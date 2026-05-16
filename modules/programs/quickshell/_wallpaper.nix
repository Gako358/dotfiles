{
  c,
  ca,
  wallpaperDir,
}:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Dialogs
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Io
  import Quickshell.Wayland

  Scope {
      id: root

      readonly property string wallpaperDir: "${wallpaperDir}"
      property string currentPath: ""
      property var files: []

      property bool opened: false
      function show()   { root.opened = true;  refreshProc.running = true }
      function hide()   { root.opened = false }
      function toggle() { if (root.opened) root.hide(); else root.show() }

      IpcHandler {
          target: "wallpaper"
          function show()    { root.show() }
          function hide()    { root.hide() }
          function toggle()  { root.toggle() }
          function refresh() { refreshProc.running = true }
          function set(path: string) { root.apply(path) }
      }

      // ── List images in the wallpaper directory ───────────
      Process {
          id: refreshProc
          running: true
          command: ["sh", "-c",
              "DIR=\"$1\"; mkdir -p \"$DIR\"; " +
              "find \"$DIR\" -maxdepth 1 -type f " +
              "\\( -iname '*.jpg'  -o -iname '*.jpeg' " +
              " -o -iname '*.png'  -o -iname '*.webp' " +
              " -o -iname '*.bmp' \\) 2>/dev/null | sort",
              "sh", root.wallpaperDir]
          stdout: StdioCollector { id: refreshOut }
          onExited: {
              var txt = (refreshOut.text || "").trim()
              root.files = txt.length > 0 ? txt.split("\n") : []
          }
      }

      // ── Apply a wallpaper via hyprpaper ──────────────────
      Process { id: applyProc }
      function apply(path) {
          if (!path) return
          var prev = root.currentPath
          applyProc.command = ["sh", "-c",
              "set -e; P=\"$1\"; PREV=\"$2\"; " +
              "hyprctl hyprpaper preload \"$P\" >/dev/null; " +
              "hyprctl hyprpaper wallpaper \",$P\" >/dev/null; " +
              "STATE=\"$HOME/.local/state/quickshell\"; " +
              "mkdir -p \"$STATE\"; " +
              "printf '%s' \"$P\" > \"$STATE/wallpaper\"; " +
              "if [ -n \"$PREV\" ] && [ \"$PREV\" != \"$P\" ]; then " +
              "  hyprctl hyprpaper unload \"$PREV\" >/dev/null 2>&1 || true; " +
              "fi",
              "sh", path, prev]
          applyProc.running = true
          root.currentPath = path
      }

      // ── Restore last-used wallpaper on shell start ───────
      Process {
          id: restoreProc
          running: true
          command: ["sh", "-c",
              "cat \"$HOME/.local/state/quickshell/wallpaper\" 2>/dev/null"]
          stdout: StdioCollector { id: restoreOut }
          onExited: {
              var p = (restoreOut.text || "").trim()
              if (p.length > 0) root.apply(p)
          }
      }

      // ── Copy a chosen file into the wallpaper dir & apply ─
      Process {
          id: addProc
          command: ["sh", "-c",
              "set -e; SRC=\"$1\"; DIR=\"$2\"; mkdir -p \"$DIR\"; " +
              "BASE=$(basename \"$SRC\"); DEST=\"$DIR/$BASE\"; " +
              "if [ -e \"$DEST\" ] && ! cmp -s \"$SRC\" \"$DEST\"; then " +
              "  EXT=\"''${BASE##*.}\"; NAME=\"''${BASE%.*}\"; " +
              "  DEST=\"$DIR/''${NAME}-$(date +%s).''${EXT}\"; " +
              "fi; " +
              "cp -f \"$SRC\" \"$DEST\"; printf '%s' \"$DEST\"",
              "sh", "", root.wallpaperDir]
          stdout: StdioCollector { id: addOut }
          onExited: {
              var dest = (addOut.text || "").trim()
              if (dest.length > 0) {
                  refreshProc.running = true
                  root.apply(dest)
              }
          }
      }
      function addFile(srcPath) {
          if (!srcPath) return
          addProc.command = ["sh", "-c",
              "set -e; SRC=\"$1\"; DIR=\"$2\"; mkdir -p \"$DIR\"; " +
              "BASE=$(basename \"$SRC\"); DEST=\"$DIR/$BASE\"; " +
              "if [ -e \"$DEST\" ] && ! cmp -s \"$SRC\" \"$DEST\"; then " +
              "  EXT=\"''${BASE##*.}\"; NAME=\"''${BASE%.*}\"; " +
              "  DEST=\"$DIR/''${NAME}-$(date +%s).''${EXT}\"; " +
              "fi; " +
              "cp -f \"$SRC\" \"$DEST\"; printf '%s' \"$DEST\"",
              "sh", srcPath, root.wallpaperDir]
          addProc.running = true
      }

      // ── Native file picker (uses xdg-desktop-portal-gtk) ─
      FileDialog {
          id: filePicker
          title: "Add wallpaper image"
          nameFilters: [
              "Image files (*.jpg *.jpeg *.png *.webp *.bmp)",
              "All files (*)"
          ]
          fileMode: FileDialog.OpenFile
          onAccepted: {
              var s = filePicker.selectedFile.toString()
              if (s.indexOf("file://") === 0) s = s.substring(7)
              root.addFile(decodeURIComponent(s))
          }
      }

      // ── Panel UI ─────────────────────────────────────────
      PanelWindow {
          id: panel
          visible: root.opened || card.opacity > 0.01

          WlrLayershell.namespace: "quickshell-wallpaper"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors { top: true; right: true }
          margins { top: 50; right: 8 }
          implicitWidth: 560
          implicitHeight: 460
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

                  // ── Header ───────────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      spacing: 8

                      Text {
                          text: "󰸉  Wallpapers"
                          color: "${c "base0D"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 14
                          font.weight: Font.Medium
                      }
                      Item { Layout.fillWidth: true }
                      Text {
                          text: root.files.length + " image"
                              + (root.files.length === 1 ? "" : "s")
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 11
                      }

                      // Add-from-file button
                      Rectangle {
                          Layout.preferredHeight: 26
                          Layout.preferredWidth: addRow.implicitWidth + 14
                          radius: 13
                          color: addHover.hovered
                              ? "${ca "base02" "cc"}"
                              : "${ca "base01" "aa"}"
                          border.width: 1
                          border.color: "${c "base02"}"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: addHover }

                          Row {
                              id: addRow
                              anchors.centerIn: parent
                              spacing: 6
                              Text {
                                  anchors.verticalCenter: parent.verticalCenter
                                  text: "󰉔"
                                  color: "${c "base0B"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 13
                              }
                              Text {
                                  anchors.verticalCenter: parent.verticalCenter
                                  text: "Add image…"
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                              }
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: filePicker.open()
                          }
                      }

                      // Refresh button
                      Rectangle {
                          Layout.preferredHeight: 26
                          Layout.preferredWidth: 26
                          radius: 13
                          color: refreshHover.hovered
                              ? "${ca "base02" "cc"}"
                              : "transparent"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: refreshHover }
                          Text {
                              anchors.centerIn: parent
                              text: "󰑐"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 13
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: refreshProc.running = true
                          }
                      }
                  }

                  Rectangle {
                      Layout.fillWidth: true
                      Layout.preferredHeight: 1
                      color: "${ca "base02" "80"}"
                  }

                  // ── Current path ─────────────────────────
                  Text {
                      visible: root.currentPath.length > 0
                      Layout.fillWidth: true
                      text: "Current: " + root.currentPath
                      color: "${c "base04"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 10
                      elide: Text.ElideMiddle
                  }

                  // ── Empty state ──────────────────────────
                  ColumnLayout {
                      visible: root.files.length === 0
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      spacing: 6
                      Item { Layout.fillHeight: true }
                      Text {
                          Layout.fillWidth: true
                          horizontalAlignment: Text.AlignHCenter
                          text: "No images in"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 12
                      }
                      Text {
                          Layout.fillWidth: true
                          horizontalAlignment: Text.AlignHCenter
                          text: root.wallpaperDir
                          color: "${c "base05"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 11
                          elide: Text.ElideMiddle
                      }
                      Text {
                          Layout.fillWidth: true
                          Layout.topMargin: 6
                          horizontalAlignment: Text.AlignHCenter
                          text: "Click  󰉔 Add image…  to add one."
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 11
                      }
                      Item { Layout.fillHeight: true }
                  }

                  // ── Thumbnail grid ───────────────────────
                  ScrollView {
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      visible: root.files.length > 0
                      clip: true

                      GridView {
                          id: grid
                          anchors.fill: parent
                          cellWidth: 168
                          cellHeight: 108
                          model: root.files
                          interactive: true
                          boundsBehavior: Flickable.StopAtBounds

                          delegate: Item {
                              required property string modelData
                              width: 164
                              height: 104

                              Rectangle {
                                  anchors.fill: parent
                                  radius: 10
                                  color: "${ca "base01" "aa"}"
                                  border.width: parent.modelData === root.currentPath ? 2 : 1
                                  border.color: parent.modelData === root.currentPath
                                      ? "${c "base0D"}"
                                      : (thumbHover.hovered
                                          ? "${c "base03"}"
                                          : "${c "base02"}")
                                  Behavior on border.color { ColorAnimation { duration: 120 } }

                                  Image {
                                      anchors.fill: parent
                                      anchors.margins: 2
                                      source: "file://" + parent.parent.modelData
                                      fillMode: Image.PreserveAspectCrop
                                      asynchronous: true
                                      cache: true
                                      sourceSize.width: 320
                                      sourceSize.height: 200
                                  }

                                  // File-name caption
                                  Rectangle {
                                      anchors.left: parent.left
                                      anchors.right: parent.right
                                      anchors.bottom: parent.bottom
                                      anchors.margins: 2
                                      height: nameText.implicitHeight + 6
                                      color: "${ca "base00" "cc"}"
                                      radius: 6
                                      Text {
                                          id: nameText
                                          anchors.fill: parent
                                          anchors.leftMargin: 6
                                          anchors.rightMargin: 6
                                          verticalAlignment: Text.AlignVCenter
                                          text: {
                                              var p = parent.parent.parent.modelData
                                              var i = p.lastIndexOf("/")
                                              return i >= 0 ? p.substring(i + 1) : p
                                          }
                                          color: "${c "base05"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 10
                                          elide: Text.ElideRight
                                      }
                                  }

                                  HoverHandler { id: thumbHover }

                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: {
                                          root.apply(parent.parent.modelData)
                                          root.hide()
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
