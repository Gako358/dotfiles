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

      property var appointments: null
      property bool opened: false

      // Form state
      property string editingId: ""        // empty => new
      property string fieldDate: ""        // YYYY-MM-DD
      property string fieldTime: "09:00"   // HH:MM
      property string fieldTitle: ""
      property string fieldBody: ""
      property int    fieldLead: 0

      readonly property var leadOptions: [
          { mins: 0,    label: "None" },
          { mins: 5,    label: "5 min" },
          { mins: 15,   label: "15 min" },
          { mins: 30,   label: "30 min" },
          { mins: 60,   label: "1 hour" },
          { mins: 120,  label: "2 hours" },
          { mins: 1440, label: "1 day" }
      ]

      function _today() {
          var d = new Date()
          return d.getFullYear().toString().padStart(4, "0")
              + "-" + (d.getMonth() + 1).toString().padStart(2, "0")
              + "-" + d.getDate().toString().padStart(2, "0")
      }

      function _resetForm(dateStr) {
          root.editingId  = ""
          root.fieldDate  = dateStr || root._today()
          root.fieldTime  = "09:00"
          root.fieldTitle = ""
          root.fieldBody  = ""
          root.fieldLead  = 0
      }

      function _loadInto(id) {
          if (!root.appointments) return
          var it = root.appointments.get(id)
          if (!it) return
          root.editingId  = it.id
          root.fieldDate  = it.date
          root.fieldTime  = it.time
          root.fieldTitle = it.title
          root.fieldBody  = it.body
          root.fieldLead  = it.leadMins
      }

      function showForDate(dateStr) {
          root._resetForm(dateStr)
          root.opened = true
      }
      function showEdit(id) {
          root._loadInto(id)
          root.opened = true
      }
      function hide()   { root.opened = false }
      function toggle() { if (root.opened) root.hide(); else root.showForDate(root._today()) }

      function _saveCurrent() {
          if (!root.appointments) return
          if (root.fieldTitle.trim().length === 0) return
          // Validate date/time loosely.
          var validDate = /^\d{4}-\d{2}-\d{2}$/.test(root.fieldDate)
          var validTime = /^\d{2}:\d{2}$/.test(root.fieldTime)
          if (!validDate || !validTime) return

          if (root.editingId.length > 0) {
              root.appointments.update(root.editingId, {
                  title: root.fieldTitle,
                  body: root.fieldBody,
                  date: root.fieldDate,
                  time: root.fieldTime,
                  leadMins: root.fieldLead
              })
          } else {
              root.appointments.add({
                  title: root.fieldTitle,
                  body: root.fieldBody,
                  date: root.fieldDate,
                  time: root.fieldTime,
                  leadMins: root.fieldLead
              })
          }
          root.hide()
      }

      function _deleteCurrent() {
          if (!root.appointments || root.editingId.length === 0) return
          root.appointments.remove(root.editingId)
          root.hide()
      }

      IpcHandler {
          target: "appointment-editor"
          function showForDate(date: string) { root.showForDate(date) }
          function showEdit(id: string)     { root.showEdit(id) }
          function hide()                   { root.hide() }
          function toggle()                 { root.toggle() }
      }

      // Live list of appointments on the currently-edited day. Re-bind via revision.
      readonly property var dayList:
          (root.appointments && root.appointments.revision >= 0)
              ? root.appointments.listForDate(root.fieldDate)
              : []

      PanelWindow {
          id: panel
          visible: root.opened || card.opacity > 0.01

          WlrLayershell.namespace: "quickshell-appointment-editor"
          WlrLayershell.layer: WlrLayer.Overlay
          WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

          anchors { top: true; bottom: true; left: true; right: true }
          color: "transparent"
          exclusionMode: ExclusionMode.Ignore

          Shortcut {
              sequences: ["Escape"]
              onActivated: root.hide()
          }

          // Outside click dismiss
          MouseArea {
              anchors.fill: parent
              anchors.bottomMargin: 48
              onClicked: root.hide()
          }

          Rectangle {
              id: card
              width: 520
              height: Math.min(660, panel.height - 80)
              anchors.bottom: parent.bottom
              anchors.right: parent.right
              anchors.bottomMargin: 52
              anchors.rightMargin: 8

              color: "${ca "base00" "e6"}"
              radius: 16
              border.width: 1
              border.color: "${c "base02"}"
              clip: true

              property real slideY: root.opened ? 0 : 18
              transform: Translate { y: card.slideY }
              transformOrigin: Item.BottomRight
              opacity: root.opened ? 1 : 0
              scale: root.opened ? 1 : 0.96
              Behavior on opacity { NumberAnimation { duration: 220; easing.type: Easing.OutCubic } }
              Behavior on scale   { NumberAnimation { duration: 280; easing.type: Easing.OutQuint } }
              Behavior on slideY  { NumberAnimation { duration: 280; easing.type: Easing.OutQuint } }

              // Swallow clicks inside card
              MouseArea { anchors.fill: parent; onClicked: {} }

              ColumnLayout {
                  anchors.fill: parent
                  anchors.margins: 16
                  spacing: 12

                  // ── Header ──────────────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      spacing: 8

                      Text {
                          text: root.editingId.length > 0
                              ? "󰃭  Edit appointment"
                              : "󰃭  New appointment"
                          color: "${c "base0D"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 15
                          font.weight: Font.Medium
                      }

                      Item { Layout.fillWidth: true }

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

                  Rectangle {
                      Layout.fillWidth: true
                      height: 1
                      color: "${c "base02"}"
                  }

                  // ── Form ───────────────────────────────────
                  ColumnLayout {
                      Layout.fillWidth: true
                      spacing: 10

                      // Title
                      Text {
                          text: "Title"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 10
                      }
                      Rectangle {
                          Layout.fillWidth: true
                          height: 34
                          radius: 8
                          color: "${ca "base01" "cc"}"
                          border.width: 1
                          border.color: titleInput.activeFocus
                              ? "${c "base0D"}"
                              : "${c "base02"}"
                          Behavior on border.color { ColorAnimation { duration: 120 } }
                          TextInput {
                              id: titleInput
                              anchors.fill: parent
                              anchors.leftMargin: 10
                              anchors.rightMargin: 10
                              verticalAlignment: TextInput.AlignVCenter
                              color: "${c "base05"}"
                              selectionColor: "${ca "base0D" "66"}"
                              selectedTextColor: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 13
                              text: root.fieldTitle
                              onTextChanged: root.fieldTitle = text
                              clip: true
                              Keys.onReturnPressed:  root._saveCurrent()
                              Keys.onEnterPressed:   root._saveCurrent()
                              Keys.onEscapePressed:  root.hide()
                          }
                          Text {
                              anchors.verticalCenter: parent.verticalCenter
                              anchors.left: parent.left
                              anchors.leftMargin: 10
                              visible: titleInput.text === "" && !titleInput.activeFocus
                              text: "What's happening?"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 13
                          }
                      }

                      // Notes
                      Text {
                          text: "Notes (optional)"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 10
                      }
                      Rectangle {
                          Layout.fillWidth: true
                          Layout.preferredHeight: 64
                          radius: 8
                          color: "${ca "base01" "cc"}"
                          border.width: 1
                          border.color: bodyInput.activeFocus
                              ? "${c "base0D"}"
                              : "${c "base02"}"
                          Behavior on border.color { ColorAnimation { duration: 120 } }
                          ScrollView {
                              anchors.fill: parent
                              anchors.margins: 6
                              TextArea {
                                  id: bodyInput
                                  background: null
                                  color: "${c "base05"}"
                                  selectionColor: "${ca "base0D" "66"}"
                                  selectedTextColor: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 12
                                  wrapMode: TextArea.Wrap
                                  text: root.fieldBody
                                  placeholderText: "Add details…"
                                  placeholderTextColor: "${c "base04"}"
                                  onTextChanged: root.fieldBody = text
                              }
                          }
                      }

                      // Date + Time row
                      RowLayout {
                          Layout.fillWidth: true
                          spacing: 12

                          // Date
                          ColumnLayout {
                              Layout.fillWidth: true
                              spacing: 4
                              Text {
                                  text: "Date  (YYYY-MM-DD)"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              Rectangle {
                                  Layout.fillWidth: true
                                  height: 34
                                  radius: 8
                                  color: "${ca "base01" "cc"}"
                                  border.width: 1
                                  border.color: dateInput.activeFocus
                                      ? "${c "base0D"}"
                                      : (dateValid ? "${c "base02"}" : "${c "base08"}")
                                  property bool dateValid: /^\d{4}-\d{2}-\d{2}$/.test(root.fieldDate)
                                  Behavior on border.color { ColorAnimation { duration: 120 } }
                                  TextInput {
                                      id: dateInput
                                      anchors.fill: parent
                                      anchors.leftMargin: 10
                                      anchors.rightMargin: 10
                                      verticalAlignment: TextInput.AlignVCenter
                                      color: "${c "base05"}"
                                      selectionColor: "${ca "base0D" "66"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 13
                                      text: root.fieldDate
                                      onTextChanged: root.fieldDate = text
                                      inputMask: "9999-99-99"
                                      Keys.onReturnPressed:  root._saveCurrent()
                                      Keys.onEnterPressed:   root._saveCurrent()
                                      Keys.onEscapePressed:  root.hide()
                                  }
                              }
                          }

                          // Time
                          ColumnLayout {
                              Layout.preferredWidth: 140
                              spacing: 4
                              Text {
                                  text: "Time  (24h)"
                                  color: "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 10
                              }
                              RowLayout {
                                  spacing: 6
                                  Rectangle {
                                      Layout.preferredWidth: 56
                                      Layout.preferredHeight: 34
                                      radius: 8
                                      color: "${ca "base01" "cc"}"
                                      border.width: 1
                                      border.color: hourInput.activeFocus
                                          ? "${c "base0D"}"
                                          : "${c "base02"}"
                                      Behavior on border.color { ColorAnimation { duration: 120 } }
                                      TextInput {
                                          id: hourInput
                                          anchors.fill: parent
                                          horizontalAlignment: TextInput.AlignHCenter
                                          verticalAlignment: TextInput.AlignVCenter
                                          color: "${c "base05"}"
                                          selectionColor: "${ca "base0D" "66"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 16
                                          font.weight: Font.Medium
                                          maximumLength: 2
                                          validator: IntValidator { bottom: 0; top: 23 }
                                          text: root.fieldTime.split(":")[0] || "00"
                                          onTextChanged: {
                                              var h = parseInt(text)
                                              if (isNaN(h)) h = 0
                                              h = Math.max(0, Math.min(23, h))
                                              var m = root.fieldTime.split(":")[1] || "00"
                                              root.fieldTime =
                                                  h.toString().padStart(2, "0") + ":" + m
                                          }
                                          Keys.onReturnPressed: root._saveCurrent()
                                          Keys.onEnterPressed:  root._saveCurrent()
                                          Keys.onEscapePressed: root.hide()
                                      }
                                  }
                                  Text {
                                      text: ":"
                                      color: "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 16
                                  }
                                  Rectangle {
                                      Layout.preferredWidth: 56
                                      Layout.preferredHeight: 34
                                      radius: 8
                                      color: "${ca "base01" "cc"}"
                                      border.width: 1
                                      border.color: minInput.activeFocus
                                          ? "${c "base0D"}"
                                          : "${c "base02"}"
                                      Behavior on border.color { ColorAnimation { duration: 120 } }
                                      TextInput {
                                          id: minInput
                                          anchors.fill: parent
                                          horizontalAlignment: TextInput.AlignHCenter
                                          verticalAlignment: TextInput.AlignVCenter
                                          color: "${c "base05"}"
                                          selectionColor: "${ca "base0D" "66"}"
                                          font.family: "RobotoMono Nerd Font"
                                          font.pixelSize: 16
                                          font.weight: Font.Medium
                                          maximumLength: 2
                                          validator: IntValidator { bottom: 0; top: 59 }
                                          text: root.fieldTime.split(":")[1] || "00"
                                          onTextChanged: {
                                              var m = parseInt(text)
                                              if (isNaN(m)) m = 0
                                              m = Math.max(0, Math.min(59, m))
                                              var h = root.fieldTime.split(":")[0] || "00"
                                              root.fieldTime =
                                                  h + ":" + m.toString().padStart(2, "0")
                                          }
                                          Keys.onReturnPressed: root._saveCurrent()
                                          Keys.onEnterPressed:  root._saveCurrent()
                                          Keys.onEscapePressed: root.hide()
                                      }
                                  }
                              }
                          }
                      }

                      // Lead time selector
                      Text {
                          text: "Remind me before"
                          color: "${c "base04"}"
                          font.family: "RobotoMono Nerd Font"
                          font.pixelSize: 10
                      }
                      Flow {
                          Layout.fillWidth: true
                          spacing: 6
                          Repeater {
                              model: root.leadOptions
                              Rectangle {
                                  required property var modelData
                                  property bool selected: root.fieldLead === modelData.mins
                                  width: chipText.implicitWidth + 18
                                  height: 26
                                  radius: 13
                                  color: selected
                                      ? "${ca "base0D" "33"}"
                                      : (chipHover.hovered
                                          ? "${ca "base02" "cc"}"
                                          : "${ca "base01" "75"}")
                                  border.width: 1
                                  border.color: selected ? "${c "base0D"}" : "${c "base02"}"
                                  Behavior on color { ColorAnimation { duration: 100 } }
                                  Behavior on border.color { ColorAnimation { duration: 100 } }
                                  HoverHandler { id: chipHover }
                                  Text {
                                      id: chipText
                                      anchors.centerIn: parent
                                      text: modelData.label
                                      color: parent.selected ? "${c "base0D"}" : "${c "base05"}"
                                      font.family: "RobotoMono Nerd Font"
                                      font.pixelSize: 11
                                      font.weight: parent.selected ? Font.Medium : Font.Normal
                                  }
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: root.fieldLead = modelData.mins
                                  }
                              }
                          }
                      }
                  }

                  Rectangle {
                      Layout.fillWidth: true
                      height: 1
                      color: "${c "base02"}"
                  }

                  // ── Existing on this day ──────────────────
                  Text {
                      text: root.dayList.length > 0
                          ? ("On " + root.fieldDate + "  (" + root.dayList.length + ")")
                          : ("Nothing else on " + root.fieldDate)
                      color: "${c "base04"}"
                      font.family: "RobotoMono Nerd Font"
                      font.pixelSize: 11
                  }

                  ListView {
                      id: dayListView
                      Layout.fillWidth: true
                      Layout.fillHeight: true
                      Layout.minimumHeight: 60
                      clip: true
                      spacing: 4
                      model: root.dayList
                      boundsBehavior: Flickable.StopAtBounds
                      ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

                      delegate: Rectangle {
                          id: existRow
                          required property int index
                          required property var modelData
                          width: dayListView.width
                          height: 36
                          radius: 8
                          color: root.editingId === existRow.modelData.id
                              ? "${ca "base0D" "22"}"
                              : (existHover.hovered
                                  ? "${ca "base01" "cc"}"
                                  : "${ca "base00" "60"}")
                          border.width: 1
                          border.color: root.editingId === existRow.modelData.id
                              ? "${c "base0D"}"
                              : "${c "base02"}"
                          Behavior on color { ColorAnimation { duration: 100 } }
                          HoverHandler { id: existHover }

                          RowLayout {
                              anchors.fill: parent
                              anchors.leftMargin: 10
                              anchors.rightMargin: 8
                              spacing: 8
                              Text {
                                  text: existRow.modelData.time
                                  color: "${c "base0C"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                                  font.weight: Font.Medium
                              }
                              Text {
                                  Layout.fillWidth: true
                                  text: existRow.modelData.title
                                  color: "${c "base05"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 12
                                  elide: Text.ElideRight
                              }
                              Text {
                                  visible: existRow.modelData.leadMins > 0
                                  text: "󰂟"
                                  color: "${c "base0A"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 11
                              }
                              Text {
                                  text: "󰏫"
                                  color: editHover.hovered ? "${c "base0D"}" : "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 12
                                  HoverHandler { id: editHover }
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: root._loadInto(existRow.modelData.id)
                                  }
                              }
                              Text {
                                  text: "󰅖"
                                  color: delHover.hovered ? "${c "base08"}" : "${c "base04"}"
                                  font.family: "RobotoMono Nerd Font"
                                  font.pixelSize: 12
                                  HoverHandler { id: delHover }
                                  MouseArea {
                                      anchors.fill: parent
                                      cursorShape: Qt.PointingHandCursor
                                      onClicked: {
                                          if (root.editingId === existRow.modelData.id)
                                              root._resetForm(root.fieldDate)
                                          if (root.appointments)
                                              root.appointments.remove(existRow.modelData.id)
                                      }
                                  }
                              }
                          }

                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              z: -1
                              onClicked: root._loadInto(existRow.modelData.id)
                          }
                      }
                  }

                  // ── Footer buttons ────────────────────────
                  RowLayout {
                      Layout.fillWidth: true
                      spacing: 8

                      // New / Cancel-edit
                      Rectangle {
                          visible: root.editingId.length > 0
                          Layout.preferredHeight: 32
                          Layout.preferredWidth: 92
                          radius: 8
                          color: newHover.hovered
                              ? "${ca "base02" "cc"}"
                              : "${ca "base01" "75"}"
                          border.width: 1
                          border.color: "${c "base02"}"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: newHover }
                          Text {
                              anchors.centerIn: parent
                              text: "New"
                              color: "${c "base05"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: root._resetForm(root.fieldDate)
                          }
                      }

                      Item { Layout.fillWidth: true }

                      // Delete (edit mode only)
                      Rectangle {
                          visible: root.editingId.length > 0
                          Layout.preferredHeight: 32
                          Layout.preferredWidth: 92
                          radius: 8
                          color: delBtnHover.hovered
                              ? "${ca "base08" "55"}"
                              : "${ca "base08" "33"}"
                          border.width: 1
                          border.color: "${c "base08"}"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: delBtnHover }
                          Text {
                              anchors.centerIn: parent
                              text: "Delete"
                              color: "${c "base08"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                              font.weight: Font.Medium
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: root._deleteCurrent()
                          }
                      }

                      // Cancel
                      Rectangle {
                          Layout.preferredHeight: 32
                          Layout.preferredWidth: 92
                          radius: 8
                          color: cancelHover.hovered
                              ? "${ca "base02" "cc"}"
                              : "${ca "base01" "75"}"
                          border.width: 1
                          border.color: "${c "base02"}"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: cancelHover }
                          Text {
                              anchors.centerIn: parent
                              text: "Cancel"
                              color: "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: Qt.PointingHandCursor
                              onClicked: root.hide()
                          }
                      }

                      // Save
                      Rectangle {
                          Layout.preferredHeight: 32
                          Layout.preferredWidth: 100
                          radius: 8
                          property bool canSave: root.fieldTitle.trim().length > 0
                              && /^\d{4}-\d{2}-\d{2}$/.test(root.fieldDate)
                              && /^\d{2}:\d{2}$/.test(root.fieldTime)
                          color: !canSave
                              ? "${ca "base02" "55"}"
                              : (saveHover.hovered
                                  ? "${ca "base0D" "66"}"
                                  : "${ca "base0D" "44"}")
                          border.width: 1
                          border.color: canSave ? "${c "base0D"}" : "${c "base02"}"
                          Behavior on color { ColorAnimation { duration: 120 } }
                          Behavior on border.color { ColorAnimation { duration: 120 } }
                          HoverHandler { id: saveHover }
                          Text {
                              anchors.centerIn: parent
                              text: root.editingId.length > 0 ? "Save" : "Add"
                              color: parent.canSave ? "${c "base0D"}" : "${c "base04"}"
                              font.family: "RobotoMono Nerd Font"
                              font.pixelSize: 11
                              font.weight: Font.Medium
                          }
                          MouseArea {
                              anchors.fill: parent
                              cursorShape: parent.canSave
                                  ? Qt.PointingHandCursor : Qt.ArrowCursor
                              enabled: parent.canSave
                              onClicked: root._saveCurrent()
                          }
                      }
                  }
              }
          }
      }
  }
''
