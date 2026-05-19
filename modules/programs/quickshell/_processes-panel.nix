{ c, ca }:
''
  import QtQuick
  import QtQuick.Controls
  import QtQuick.Layouts
  import Quickshell
  import Quickshell.Wayland
  import Quickshell.Io

  Scope {
      id: root

      property bool opened: false
      property string initialSort: "cpu"   // "cpu" or "mem"

      function show()        { root.opened = true;  root.refresh() }
      function hide()        { root.opened = false }
      function toggle()      { if (root.opened) root.hide(); else root.show() }
      function showSortedBy(mode) {
          sortMode = mode
          root.show()
      }

      IpcHandler {
          target: "processes"
          function show()   { root.show() }
          function hide()   { root.hide() }
          function toggle() { root.toggle() }
      }

      // ── State ─────────────────────────────────────────────────
      property string sortMode: "cpu"   // "cpu" or "mem"
      property string searchText: ""
      property var    allProcs: []
      property int    killPid:  -1      // pid awaiting confirmation

      // Filtered list recomputed whenever allProcs or searchText changes
      property var filteredProcs: []
      onAllProcsChanged:  root.refilter()
      onSearchTextChanged: root.refilter()

      function refilter() {
          var q = root.searchText.toLowerCase().trim()
          if (q === "") {
              root.filteredProcs = root.allProcs.slice()
              return
          }
          var out = []
          for (var i = 0; i < root.allProcs.length; ++i) {
              if (root.allProcs[i].name.toLowerCase().indexOf(q) !== -1)
                  out.push(root.allProcs[i])
          }
          root.filteredProcs = out
      }

      function refresh() {
          psProc.running = true
      }

      // ── Process fetcher ────────────────────────────────────────
      Process {
          id: psProc
          // Sort flag changes dynamically based on sortMode
          command: ["sh", "-c",
              "ps -eo pid,comm,%cpu,%mem --sort=-%" + root.sortMode + " --no-headers | head -n 60"]
          stdout: StdioCollector { id: psOut }
          onExited: {
              var lines = (psOut.text || "").trim().split("\n")
              var arr = []
              for (var i = 0; i < lines.length; ++i) {
                  var f = lines[i].trim().split(/\s+/)
                  if (f.length < 4) continue
                  var pid  = parseInt(f[0]) || 0
                  var name = f[1]
                  var cpu  = parseFloat(f[2]) || 0
                  var mem  = parseFloat(f[3]) || 0
                  if (pid === 0) continue
                  arr.push({ pid: pid, name: name, cpu: cpu, mem: mem })
              }
              root.allProcs = arr
          }
      }

      // Kill process
      Process {
          id: killProc
          property int targetPid: -1
          command: ["kill", "-9", String(targetPid)]
          onExited: {
              root.killPid = -1
              root.refresh()
          }
      }

      Timer {
          running: root.opened
          repeat: true
          interval: 2000
          triggeredOnStart: true
          onTriggered: root.refresh()
      }

      // ── Panel window ───────────────────────────────────────────
      PanelWindow {
          id: panel
          visible: root.opened || procCard.opacity > 0.01

          WlrLayershell.namespace: "quickshell-processes"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors { top: true; bottom: true; left: true; right: true }
          color: "transparent"
          exclusionMode: ExclusionMode.Ignore

          Shortcut {
              sequences: ["Escape"]
              onActivated: {
                  if (root.killPid !== -1) {
                      root.killPid = -1
                  } else {
                      root.hide()
                  }
              }
          }

          MouseArea {
              anchors.fill: parent
              onClicked: function(mouse) {
                  if (root.killPid !== -1) {
                      root.killPid = -1
                      return
                  }
                  root.hide()
              }
          }

          // ── Card ──────────────────────────────────────────────
          Rectangle {
              id: procCard
              width: 460
              // Fixed reasonable height, capped to the screen so it never overflows
              height: Math.min(560, panel.height - 80)
              anchors.bottom: parent.bottom
              anchors.right: parent.right
              anchors.bottomMargin: 52
              anchors.rightMargin: 8

              color: "${ca "base00" "e6"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"
              clip: true

              // Slide up from the bar on open (matches dashboard behavior)
              property real slideY: root.opened ? 0 : 18
              transform: Translate { y: procCard.slideY }
              transformOrigin: Item.BottomRight
              opacity: root.opened ? 1 : 0
              scale: root.opened ? 1 : 0.96

              Behavior on opacity  { NumberAnimation { duration: 220; easing.type: Easing.OutCubic } }
              Behavior on scale    { NumberAnimation { duration: 280; easing.type: Easing.OutQuint } }
              Behavior on slideY   { NumberAnimation { duration: 280; easing.type: Easing.OutQuint } }

              MouseArea {
                  anchors.fill: parent
                  onClicked: {} // swallow — don't dismiss
              }

              ColumnLayout {
                  anchors.fill: parent
                  anchors.margins: 16
                  spacing: 12

                  // ── Header ──────────────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      spacing: 8

                      Text {
                          text: "󰓅 Processes"
                          color: "${c "base0C"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 15
                          font.weight: Font.Medium
                      }

                      Item { Layout.fillWidth: true }

                      // Sort toggle buttons
                      RowLayout {
                          spacing: 4
                          Repeater {
                              model: [
                                  { label: "CPU", mode: "cpu" },
                                  { label: "MEM", mode: "mem" }
                              ]
                              Rectangle {
                                  required property var modelData
                                  property bool active: root.sortMode === modelData.mode
                                  width: 44
                                  height: 22
                                  radius: 6
                                  color: active
                                      ? "${ca "base0C" "33"}"
                                      : (sortToggleHover.hovered ? "${ca "base02" "cc"}" : "${ca "base01" "75"}")
                                  border.width: 1
                                  border.color: active ? "${c "base0C"}" : "${c "base02"}"
                                  Behavior on color { ColorAnimation { duration: 100 } }
                                  HoverHandler { id: sortToggleHover }
                                  Text {
                                      anchors.centerIn: parent
                                      text: modelData.label
                                      color: parent.active ? "${c "base0C"}" : "${c "base04"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 10
                                      font.weight: Font.Medium
                                  }
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: {
                                          root.sortMode = modelData.mode
                                          root.refresh()
                                      }
                                  }
                              }
                          }
                      }

                      // Close button
                      Text {
                          text: "󰅖"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 16
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: root.hide()
                          }
                      }
                  }

                  // ── Search field ─────────────────────────────
                  Rectangle {
                      Layout.fillWidth: true
                      height: 34
                      radius: 8
                      color: "${ca "base01" "cc"}"
                      border.width: 1
                      border.color: searchInput.activeFocus
                          ? "${c "base0C"}"
                          : "${c "base02"}"
                      Behavior on border.color { ColorAnimation { duration: 120 } }

                      RowLayout {
                          anchors.fill: parent
                          anchors.leftMargin: 10
                          anchors.rightMargin: 8
                          spacing: 8

                          Text {
                              text: "󰍉"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 14
                          }

                          Item {
                              Layout.fillWidth: true
                              Layout.preferredHeight: searchInput.implicitHeight

                              TextInput {
                                  id: searchInput
                                  anchors.fill: parent
                                  verticalAlignment: TextInput.AlignVCenter
                                  color: "${c "base05"}"
                                  selectionColor: "${ca "base0C" "66"}"
                                  selectedTextColor: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 12
                                  clip: true
                                  onTextChanged: root.searchText = text
                                  Keys.onEscapePressed: {
                                      if (text !== "") {
                                          text = ""
                                      } else {
                                          root.hide()
                                      }
                                  }
                              }

                              // Placeholder text (manual — TextInput has no built-in placeholder)
                              Text {
                                  anchors.verticalCenter: parent.verticalCenter
                                  anchors.left: parent.left
                                  visible: searchInput.text === "" && !searchInput.activeFocus
                                  text: "Search processes…"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 12
                              }
                          }

                          // Clear search button
                          Text {
                              visible: searchInput.text !== ""
                              text: "󰅖"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 12
                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: searchInput.text = ""
                              }
                          }
                      }
                  }

                  // ── Column headers ───────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      anchors.leftMargin: 4
                      anchors.rightMargin: 4

                      Text {
                          Layout.preferredWidth: 46
                          text: "PID"
                          color: "${c "base03"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 10
                          font.weight: Font.Medium
                      }
                      Text {
                          Layout.fillWidth: true
                          text: "NAME"
                          color: "${c "base03"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 10
                          font.weight: Font.Medium
                      }
                      Text {
                          Layout.preferredWidth: 40
                          text: "CPU%"
                          horizontalAlignment: Text.AlignRight
                          color: root.sortMode === "cpu" ? "${c "base0C"}" : "${c "base03"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 10
                          font.weight: Font.Medium
                      }
                      Text {
                          Layout.preferredWidth: 40
                          text: "MEM%"
                          horizontalAlignment: Text.AlignRight
                          color: root.sortMode === "mem" ? "${c "base0E"}" : "${c "base03"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 10
                          font.weight: Font.Medium
                      }
                      // Space for kill button column
                      Item { Layout.preferredWidth: 28 }
                  }

                  Rectangle {
                      Layout.fillWidth: true
                      height: 1
                      color: "${c "base02"}"
                  }

                  // ── Process list ─────────────────────────────
                  ListView {
                      id: procList
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      clip: true
                      spacing: 2
                      boundsBehavior: Flickable.StopAtBounds
                      ScrollBar.vertical: ScrollBar {
                          policy: ScrollBar.AsNeeded
                          contentItem: Rectangle {
                              implicitWidth: 4
                              radius: 2
                              color: "${ca "base04" "80"}"
                          }
                      }

                      model: root.filteredProcs

                      delegate: Rectangle {
                          id: procRow
                          required property int index
                          required property var modelData

                          width: procList.width
                          height: 32
                          radius: 6
                          color: root.killPid === modelData.pid
                              ? "${ca "base08" "22"}"
                              : (rowHover.hovered ? "${ca "base01" "cc"}" : "transparent")
                          border.width: root.killPid === modelData.pid ? 1 : 0
                          border.color: "${c "base08"}"
                          Behavior on color { ColorAnimation { duration: 80 } }

                          HoverHandler { id: rowHover }

                          RowLayout {
                              anchors.fill: parent
                              anchors.leftMargin: 4
                              anchors.rightMargin: 4
                              spacing: 0

                              // PID
                              Text {
                                  Layout.preferredWidth: 46
                                  text: procRow.modelData.pid
                                  color: "${c "base03"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                              }

                              // Name
                              Text {
                                  Layout.fillWidth: true
                                  text: procRow.modelData.name
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 12
                                  elide: Text.ElideRight
                              }

                              // CPU%
                              Text {
                                  Layout.preferredWidth: 40
                                  text: procRow.modelData.cpu.toFixed(1)
                                  horizontalAlignment: Text.AlignRight
                                  color: procRow.modelData.cpu > 50
                                      ? "${c "base08"}"
                                      : (procRow.modelData.cpu > 20
                                          ? "${c "base0A"}"
                                          : "${c "base0C"}")
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                              }

                              // MEM%
                              Text {
                                  Layout.preferredWidth: 40
                                  text: procRow.modelData.mem.toFixed(1)
                                  horizontalAlignment: Text.AlignRight
                                  color: procRow.modelData.mem > 20
                                      ? "${c "base08"}"
                                      : (procRow.modelData.mem > 10
                                          ? "${c "base0A"}"
                                          : "${c "base0E"}")
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                              }

                              // Kill / Confirm button
                              Item {
                                  Layout.preferredWidth: 28

                                  // Normal kill button (shown on hover)
                                  Rectangle {
                                      id: killBtn
                                      anchors.centerIn: parent
                                      width: 22
                                      height: 22
                                      radius: 5
                                      visible: rowHover.hovered && root.killPid !== procRow.modelData.pid
                                      color: killBtnHover.hovered
                                          ? "${ca "base08" "33"}"
                                          : "transparent"
                                      border.width: killBtnHover.hovered ? 1 : 0
                                      border.color: "${c "base08"}"
                                      Behavior on color { ColorAnimation { duration: 80 } }
                                      HoverHandler { id: killBtnHover }

                                      Text {
                                          anchors.centerIn: parent
                                          text: "󰅖"
                                          color: "${c "base08"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 12
                                      }

                                      MouseArea {
                                          anchors.fill: parent
                                          cursorShape: Qt.PointingHandCursor
                                          onClicked: root.killPid = procRow.modelData.pid
                                      }
                                  }

                                  // Confirm kill (shown when this row is pending)
                                  Rectangle {
                                      id: confirmBtn
                                      anchors.centerIn: parent
                                      width: 22
                                      height: 22
                                      radius: 5
                                      visible: root.killPid === procRow.modelData.pid
                                      color: confirmBtnHover.hovered
                                          ? "${ca "base08" "55"}"
                                          : "${ca "base08" "33"}"
                                      border.width: 1
                                      border.color: "${c "base08"}"
                                      Behavior on color { ColorAnimation { duration: 80 } }
                                      HoverHandler { id: confirmBtnHover }

                                      Text {
                                          anchors.centerIn: parent
                                          text: "󰔑"
                                          color: "${c "base08"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 12
                                      }

                                      MouseArea {
                                          anchors.fill: parent
                                          cursorShape: Qt.PointingHandCursor
                                          onClicked: {
                                              killProc.targetPid = root.killPid
                                              killProc.running = true
                                          }
                                      }
                                  }
                              }
                          }
                      }

                      // Empty state
                      Text {
                          anchors.centerIn: parent
                          visible: root.filteredProcs.length === 0
                          text: root.searchText !== "" ? "No matching processes" : "Loading…"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 12
                      }
                  }

                  // ── Kill confirmation banner ──────────────────
                  Rectangle {
                      Layout.fillWidth: true
                      height: 48
                      radius: 8
                      visible: root.killPid !== -1
                      color: "${ca "base08" "1a"}"
                      border.width: 1
                      border.color: "${c "base08"}"

                      RowLayout {
                          anchors.fill: parent
                          anchors.leftMargin: 12
                          anchors.rightMargin: 12
                          spacing: 8

                          Text {
                              Layout.fillWidth: true
                              text: {
                                  if (root.killPid === -1) return ""
                                  var p = root.allProcs
                                  for (var i = 0; i < p.length; ++i)
                                      if (p[i].pid === root.killPid)
                                          return "Kill \u201c" + p[i].name + "\u201d (PID " + root.killPid + ")?"
                                  return "Kill PID " + root.killPid + "?"
                              }
                              color: "${c "base08"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                              wrapMode: Text.Wrap
                          }

                          // Cancel
                          Rectangle {
                              width: 60
                              height: 28
                              radius: 6
                              color: cancelHover.hovered
                                  ? "${ca "base02" "cc"}"
                                  : "${ca "base01" "75"}"
                              border.width: 1
                              border.color: "${c "base02"}"
                              Behavior on color { ColorAnimation { duration: 80 } }
                              HoverHandler { id: cancelHover }
                              Text {
                                  anchors.centerIn: parent
                                  text: "Cancel"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: root.killPid = -1
                              }
                          }

                          // Confirm
                          Rectangle {
                              width: 52
                              height: 28
                              radius: 6
                              color: killConfirmHover.hovered
                                  ? "${ca "base08" "55"}"
                                  : "${ca "base08" "33"}"
                              border.width: 1
                              border.color: "${c "base08"}"
                              Behavior on color { ColorAnimation { duration: 80 } }
                              HoverHandler { id: killConfirmHover }
                              Text {
                                  anchors.centerIn: parent
                                  text: "Kill"
                                  color: "${c "base08"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                                  font.weight: Font.Medium
                              }
                              MouseArea {
                                  anchors.fill: parent
                                  cursorShape: Qt.PointingHandCursor
                                  onClicked: {
                                      killProc.targetPid = root.killPid
                                      killProc.running = true
                                  }
                              }
                          }
                      }
                  }

                  // ── Footer: process count ────────────────────
                  Text {
                      Layout.fillWidth: true
                      text: {
                          var total = root.allProcs.length
                          var shown = root.filteredProcs.length
                          if (root.searchText !== "")
                              return shown + " / " + total + " processes"
                          return total + " processes"
                      }
                      color: "${c "base03"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 10
                      horizontalAlignment: Text.AlignRight
                  }
              }
          }
      }
  }
''
