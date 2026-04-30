_: {
  flake.nixosModules.programs-quickshell =
    {
      lib,
      config,
      ...
    }:
    {
      config = lib.mkIf (config.environment.desktop.windowManager == "hyprland") {
        security.pam.services.quickshell = { };
      };
    };

  flake.homeModules.programs-quickshell =
    {
      osConfig,
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (config.colorScheme) palette;

      c = name: "#${palette.${name}}";
      ca = name: alpha: "#${alpha}${palette.${name}}";

      shellQml = ''
        import QtQuick
        import Quickshell

        Scope {
            id: root

            Notifications { id: notifications }
            Lock          { id: lock }
            Launcher      { id: launcher }
            Session       { id: session; lockComponent: lock }
            Dashboard     {
                id: dashboard
                notifications: notifications
            }

            Variants {
                model: Quickshell.screens

                Bar {
                    required property var modelData
                    screen: modelData
                    onLauncherRequested: launcher.toggle()
                    onDashboardRequested: dashboard.toggle()
                    onCalendarRequested: dashboard.show()
                    onSessionRequested: session.toggle()
                    onSystemMonitorRequested: Quickshell.execDetached(["gnome-system-monitor"])
                    onAudioRequested: Quickshell.execDetached(["pavucontrol"])
                    onNetworkRequested: Quickshell.execDetached(["nm-connection-editor"])
                }
            }
        }
      '';

      barQml = ''
        import QtQuick
        import QtQuick.Layouts
        import Quickshell
        import Quickshell.Hyprland
        import Quickshell.Services.SystemTray
        import Quickshell.Wayland

        PanelWindow {
            id: bar

            signal launcherRequested()
            signal dashboardRequested()
            signal calendarRequested()
            signal sessionRequested()
            signal systemMonitorRequested()
            signal audioRequested()
            signal networkRequested()

            WlrLayershell.namespace: "quickshell-bar"

            readonly property bool isFocused:
                Hyprland.focusedMonitor !== null
                && bar.screen !== null
                && Hyprland.focusedMonitor.name === bar.screen.name

            anchors {
                top: true
                left: true
                right: true
            }
            margins {
                top: 4
                left: 8
                right: 8
            }

            implicitHeight: 36
            exclusiveZone: 40
            color: "transparent"

            Rectangle {
                anchors.fill: parent
                color: "${ca "base00" "75"}"
                radius: 12
                border.width: 1
                border.color: "${c "base02"}"

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: 14
                    anchors.rightMargin: 14
                    spacing: 14

                    // ── Launcher (NixOS Material Design icon) ────────
                    Item {
                        Layout.preferredWidth: 26
                        Layout.fillHeight: true
                        Text {
                            anchors.centerIn: parent
                            text: "󱄅"
                            font.family: "RobotoMono Nerd Font"
                            font.pixelSize: 18
                            color: "${c "base0D"}"
                        }
                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: bar.launcherRequested()
                        }
                    }

                    // ── Workspaces ───────────────────────────────────
                    Row {
                        Layout.alignment: Qt.AlignVCenter
                        spacing: 6
                        Repeater {
                            model: 9
                            Rectangle {
                                id: ws
                                property int wsId: index + 1
                                property bool isActive: Hyprland.focusedWorkspace
                                    && Hyprland.focusedWorkspace.id === wsId
                                width: isActive ? 24 : 10
                                height: 10
                                radius: 5
                                color: isActive
                                    ? "${c "base0D"}"
                                    : "${c "base04"}"
                                Behavior on width {
                                    NumberAnimation { duration: 220; easing.type: Easing.OutCubic }
                                }
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: Hyprland.dispatch("workspace " + ws.wsId)
                                }
                            }
                        }
                    }

                    Item { Layout.fillWidth: true }

                    // ── Clock (center) ───────────────────────────────
                    Text {
                        id: clockText
                        visible: bar.isFocused
                        text: ""
                        font.family: "RobotoMono Nerd Font"
                        font.pixelSize: 13
                        color: "${c "base05"}"
                        Timer {
                            running: true
                            repeat: true
                            interval: 1000
                            triggeredOnStart: true
                            onTriggered: clockText.text =
                                "󰥔 " + Qt.formatDateTime(new Date(), "ddd, dd MMM, hh:mm AP")
                        }
                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: bar.calendarRequested()
                        }
                    }

                    Item { Layout.fillWidth: true }

                    // ── System tray ──────────────────────────────────
                    Row {
                        visible: bar.isFocused
                        Layout.alignment: Qt.AlignVCenter
                        spacing: 8
                        Repeater {
                            model: SystemTray.items
                            Item {
                                required property SystemTrayItem modelData
                                width: 18
                                height: 18
                                Image {
                                    anchors.fill: parent
                                    source: modelData.icon
                                    fillMode: Image.PreserveAspectFit
                                    sourceSize.width: 18
                                    sourceSize.height: 18
                                }
                                MouseArea {
                                    anchors.fill: parent
                                    acceptedButtons: Qt.LeftButton | Qt.RightButton
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: function(mouse) {
                                        if (mouse.button === Qt.LeftButton)
                                            modelData.activate();
                                        else
                                            modelData.secondaryActivate();
                                    }
                                }
                            }
                        }
                    }

                    // ── Status pill ──────────────────────────────────
                    Rectangle {
                        visible: bar.isFocused
                        Layout.alignment: Qt.AlignVCenter
                        Layout.preferredHeight: 24
                        Layout.preferredWidth: statusRow.implicitWidth + 16
                        color: "${ca "base00" "75"}"
                        border.color: "${c "base02"}"
                        border.width: 1
                        radius: 8

                        Row {
                            id: statusRow
                            anchors.centerIn: parent
                            spacing: 12

                            Text {
                                text: "󰍛"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 14
                                color: "${c "base0C"}"
                                anchors.verticalCenter: parent.verticalCenter
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: bar.systemMonitorRequested()
                                }
                            }
                            Text {
                                text: "󰕾"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 14
                                color: "${c "base0E"}"
                                anchors.verticalCenter: parent.verticalCenter
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: bar.audioRequested()
                                }
                            }
                            Text {
                                text: "󰖩"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 14
                                color: "${c "base0B"}"
                                anchors.verticalCenter: parent.verticalCenter
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: bar.networkRequested()
                                }
                            }
                        }
                    }

                    // ── Dashboard toggle ─────────────────────────────
                    Text {
                        visible: bar.isFocused
                        text: "󰕮"
                        font.family: "RobotoMono Nerd Font"
                        font.pixelSize: 14
                        color: "${c "base0A"}"
                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: bar.dashboardRequested()
                        }
                    }

                    // ── Session / power menu ─────────────────────────
                    Text {
                        visible: bar.isFocused
                        text: "󰐥"
                        font.family: "RobotoMono Nerd Font"
                        font.pixelSize: 14
                        color: "${c "base08"}"
                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: bar.sessionRequested()
                        }
                    }
                }
            }
        }
      '';

      dashboardQml = ''
        import QtQuick
        import QtQuick.Controls
        import QtQuick.Layouts
        import Quickshell
        import Quickshell.Wayland
        import Quickshell.Io
        import Quickshell.Services.Mpris

        Scope {
            id: root

            // Provided by shell.qml so we can list recent notifications.
            property var notifications: null

            property bool opened: false

            function toggle() { root.opened = !root.opened }
            function show()   { root.opened = true }
            function hide()   { root.opened = false }

            IpcHandler {
                target: "dashboard"
                function toggle() { root.toggle() }
                function show()   { root.show() }
                function hide()   { root.hide() }
            }

            property string cpuPct:  "—"
            property string memUsed: "—"
            property string uptime:  "—"

            Process {
                id: cpuProc
                command: ["sh", "-c",
                    "top -bn1 | awk '/Cpu/ { printf \"%.0f\", 100 - $8 }'"]
                stdout: StdioCollector { id: cpuOut }
                onExited: root.cpuPct = (cpuOut.text || "").trim() + "%"
            }
            Process {
                id: memProc
                command: ["sh", "-c",
                    "free -h --si | awk '/Mem:/ { print $3 \" / \" $2 }'"]
                stdout: StdioCollector { id: memOut }
                onExited: root.memUsed = (memOut.text || "").trim()
            }
            Process {
                id: upProc
                command: ["sh", "-c",
                    "awk '{u=int($1); d=int(u/86400); h=int((u%86400)/3600); m=int((u%3600)/60); s=\"\"; if(d>0) s=s d\"d \"; if(h>0) s=s h\"h \"; s=s m\"m\"; print s}' /proc/uptime"]
                stdout: StdioCollector { id: upOut }
                onExited: root.uptime = (upOut.text || "").trim() || "—"
            }
            Timer {
                id: pollTimer
                running: root.opened
                repeat: true
                interval: 2000
                triggeredOnStart: true
                onTriggered: {
                    cpuProc.running = true
                    memProc.running = true
                    upProc.running = true
                }
            }

            PanelWindow {
                id: panel
                visible: root.opened || dashCard.opacity > 0.01

                WlrLayershell.namespace: "quickshell-dashboard"
                WlrLayershell.layer: WlrLayer.Overlay
                WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

                anchors {
                    top: true
                    right: true
                    bottom: true
                }
                margins {
                    top: 50
                    right: 8
                    bottom: 8
                }
                implicitWidth: 420
                color: "transparent"

                Shortcut {
                    sequences: ["Escape"]
                    onActivated: root.hide()
                }

                Rectangle {
                    id: dashCard
                    anchors.fill: parent
                    color: "${ca "base00" "e6"}"
                    radius: 16
                    border.width: 1
                    border.color: "${c "base02"}"

                    transformOrigin: Item.TopRight
                    opacity: root.opened ? 1 : 0
                    scale: root.opened ? 1 : 0.97
                    Behavior on opacity {
                        NumberAnimation { duration: 220; easing.type: Easing.OutCubic }
                    }
                    Behavior on scale {
                        NumberAnimation { duration: 240; easing.type: Easing.OutCubic }
                    }

                    ColumnLayout {
                        anchors.fill: parent
                        anchors.margins: 18
                        spacing: 16

                        // ── Greeting / Time ──────────────────────────
                        ColumnLayout {
                            spacing: 2
                            Text {
                                id: bigTime
                                text: ""
                                color: "${c "base05"}"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 38
                                font.weight: Font.Light
                                Timer {
                                    running: true; repeat: true; interval: 1000; triggeredOnStart: true
                                    onTriggered: bigTime.text =
                                        Qt.formatDateTime(new Date(), "hh:mm")
                                }
                            }
                            Text {
                                id: bigDate
                                text: ""
                                color: "${c "base0D"}"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 13
                                Timer {
                                    running: true; repeat: true; interval: 30000; triggeredOnStart: true
                                    onTriggered: bigDate.text =
                                        Qt.formatDateTime(new Date(), "dddd, MMMM d")
                                }
                            }
                        }

                        // ── Media player (MPRIS) ─────────────────────
                        Rectangle {
                            id: mediaCard
                            Layout.fillWidth: true
                            Layout.preferredHeight: 120
                            color: "${ca "base01" "75"}"
                            radius: 12
                            border.width: 1
                            border.color: "${c "base02"}"

                            property var player: Mpris.players && Mpris.players.values.length > 0
                                ? Mpris.players.values[0]
                                : null

                            // trackArtists is a Qt list, not a JS array — manual join.
                            function artistsText() {
                                if (!mediaCard.player) return ""
                                var arts = mediaCard.player.trackArtists
                                if (!arts) return ""
                                var out = []
                                for (var i = 0; i < arts.length; ++i) out.push(arts[i])
                                return out.join(", ")
                            }

                            ColumnLayout {
                                anchors.fill: parent
                                anchors.margins: 12
                                spacing: 4

                                Text {
                                    Layout.fillWidth: true
                                    text: mediaCard.player
                                        ? (mediaCard.player.trackTitle || "—")
                                        : "Nothing playing"
                                    color: "${c "base05"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 14
                                    elide: Text.ElideRight
                                }
                                Text {
                                    Layout.fillWidth: true
                                    text: mediaCard.artistsText()
                                    color: "${c "base04"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 11
                                    elide: Text.ElideRight
                                }

                                Item { Layout.fillHeight: true }

                                RowLayout {
                                    Layout.alignment: Qt.AlignHCenter
                                    spacing: 24

                                    Text {
                                        text: "󰒮"
                                        color: "${c "base0D"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 20
                                        MouseArea {
                                            anchors.fill: parent
                                            cursorShape: Qt.PointingHandCursor
                                            onClicked: if (mediaCard.player) mediaCard.player.previous()
                                        }
                                    }
                                    Text {
                                        text: mediaCard.player
                                            && mediaCard.player.playbackState === MprisPlaybackState.Playing
                                            ? "󰏤"
                                            : "󰐊"
                                        color: "${c "base0D"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 24
                                        MouseArea {
                                            anchors.fill: parent
                                            cursorShape: Qt.PointingHandCursor
                                            onClicked: if (mediaCard.player) mediaCard.player.togglePlaying()
                                        }
                                    }
                                    Text {
                                        text: "󰒭"
                                        color: "${c "base0D"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 20
                                        MouseArea {
                                            anchors.fill: parent
                                            cursorShape: Qt.PointingHandCursor
                                            onClicked: if (mediaCard.player) mediaCard.player.next()
                                        }
                                    }
                                }
                            }
                        }

                        // ── System info card ─────────────────────────
                        Rectangle {
                            Layout.fillWidth: true
                            Layout.preferredHeight: 110
                            color: "${ca "base01" "75"}"
                            radius: 12
                            border.width: 1
                            border.color: "${c "base02"}"

                            ColumnLayout {
                                anchors.fill: parent
                                anchors.margins: 14
                                spacing: 8

                                RowLayout {
                                    Layout.fillWidth: true
                                    Text {
                                        text: "󰻠 CPU"; color: "${c "base0C"}"
                                        font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                    }
                                    Item { Layout.fillWidth: true }
                                    Text {
                                        text: root.cpuPct
                                        color: "${c "base05"}"
                                        font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                    }
                                }
                                RowLayout {
                                    Layout.fillWidth: true
                                    Text {
                                        text: "󰍛 Memory"; color: "${c "base0E"}"
                                        font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                    }
                                    Item { Layout.fillWidth: true }
                                    Text {
                                        text: root.memUsed
                                        color: "${c "base05"}"
                                        font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                    }
                                }
                                RowLayout {
                                    Layout.fillWidth: true
                                    Text {
                                        text: "󰅐 Uptime"; color: "${c "base0A"}"
                                        font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                    }
                                    Item { Layout.fillWidth: true }
                                    Text {
                                        text: root.uptime
                                        color: "${c "base05"}"
                                        font.family: "RobotoMono Nerd Font"; font.pixelSize: 13
                                        elide: Text.ElideRight
                                        horizontalAlignment: Text.AlignRight
                                    }
                                }
                            }
                        }

                        // ── Recent notifications ─────────────────────
                        Rectangle {
                            id: notifCard
                            Layout.fillWidth: true
                            Layout.preferredHeight: 200
                            color: "${ca "base01" "75"}"
                            radius: 12
                            border.width: 1
                            border.color: "${c "base02"}"

                            readonly property var histModel:
                                root.notifications ? root.notifications.historyModel : null

                            ColumnLayout {
                                anchors.fill: parent
                                anchors.margins: 12
                                spacing: 6

                                RowLayout {
                                    Layout.fillWidth: true
                                    Text {
                                        text: "󰂚 Notifications"
                                        color: "${c "base0A"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 13
                                        font.weight: Font.Medium
                                    }
                                    Item { Layout.fillWidth: true }
                                    Text {
                                        visible: notifCard.histModel
                                            && notifCard.histModel.count > 0
                                        text: "Clear all"
                                        color: "${c "base04"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 10
                                        MouseArea {
                                            anchors.fill: parent
                                            cursorShape: Qt.PointingHandCursor
                                            onClicked: if (root.notifications)
                                                root.notifications.clearHistory()
                                        }
                                    }
                                }

                                Text {
                                    Layout.fillWidth: true
                                    Layout.fillHeight: true
                                    visible: !notifCard.histModel
                                        || notifCard.histModel.count === 0
                                    horizontalAlignment: Text.AlignHCenter
                                    verticalAlignment: Text.AlignVCenter
                                    text: "No new notifications"
                                    color: "${c "base04"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 11
                                }

                                ListView {
                                    id: notifList
                                    Layout.fillWidth: true
                                    Layout.fillHeight: true
                                    visible: notifCard.histModel
                                        && notifCard.histModel.count > 0
                                    clip: true
                                    spacing: 6
                                    model: notifCard.histModel
                                    boundsBehavior: Flickable.StopAtBounds
                                    ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

                                    delegate: Rectangle {
                                        required property int index
                                        required property string summary
                                        required property string body
                                        required property string appName
                                        required property string time
                                        required property int urgency

                                        width: notifList.width
                                        height: row.implicitHeight + 12
                                        color: "${ca "base00" "80"}"
                                        radius: 8
                                        border.width: 1
                                        border.color: urgency === 2
                                            ? "${c "base08"}"
                                            : "${c "base02"}"

                                        RowLayout {
                                            id: row
                                            anchors.fill: parent
                                            anchors.margins: 8
                                            spacing: 8

                                            ColumnLayout {
                                                Layout.fillWidth: true
                                                spacing: 1
                                                RowLayout {
                                                    Layout.fillWidth: true
                                                    spacing: 6
                                                    Text {
                                                        text: appName
                                                        color: "${c "base0D"}"
                                                        font.family: "RobotoMono Nerd Font"
                                                        font.pixelSize: 10
                                                        font.weight: Font.Medium
                                                        elide: Text.ElideRight
                                                        Layout.maximumWidth: 140
                                                    }
                                                    Item { Layout.fillWidth: true }
                                                    Text {
                                                        text: time
                                                        color: "${c "base04"}"
                                                        font.family: "RobotoMono Nerd Font"
                                                        font.pixelSize: 10
                                                    }
                                                }
                                                Text {
                                                    Layout.fillWidth: true
                                                    text: summary
                                                    color: "${c "base05"}"
                                                    font.family: "RobotoMono Nerd Font"
                                                    font.pixelSize: 12
                                                    elide: Text.ElideRight
                                                }
                                                Text {
                                                    Layout.fillWidth: true
                                                    visible: body !== ""
                                                    text: body
                                                    color: "${c "base06"}"
                                                    linkColor: "${c "base0C"}"
                                                    font.family: "RobotoMono Nerd Font"
                                                    font.pixelSize: 11
                                                    wrapMode: Text.Wrap
                                                    textFormat: Text.RichText
                                                    maximumLineCount: 2
                                                    elide: Text.ElideRight
                                                    onLinkActivated: function(link) {
                                                        Qt.openUrlExternally(link)
                                                    }
                                                }
                                            }

                                            Text {
                                                text: "󰅖"
                                                color: "${c "base04"}"
                                                font.family: "RobotoMono Nerd Font"
                                                font.pixelSize: 12
                                                Layout.alignment: Qt.AlignTop
                                                MouseArea {
                                                    anchors.fill: parent
                                                    cursorShape: Qt.PointingHandCursor
                                                    onClicked: if (root.notifications)
                                                        root.notifications.removeHistory(index)
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // ── Calendar (simple month grid) ─────────────
                        Rectangle {
                            id: calCard
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            color: "${ca "base01" "75"}"
                            radius: 12
                            border.width: 1
                            border.color: "${c "base02"}"

                            property date today: new Date()
                            property int year:  today.getFullYear()
                            property int month: today.getMonth()

                            ColumnLayout {
                                anchors.fill: parent
                                anchors.margins: 12
                                spacing: 6

                                Text {
                                    Layout.alignment: Qt.AlignHCenter
                                    text: Qt.formatDateTime(calCard.today, "MMMM yyyy")
                                    color: "${c "base0D"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 13
                                }

                                GridLayout {
                                    Layout.fillWidth: true
                                    Layout.fillHeight: true
                                    columns: 7
                                    rowSpacing: 2
                                    columnSpacing: 2

                                    Repeater {
                                        model: ["M","T","W","T","F","S","S"]
                                        Text {
                                            Layout.fillWidth: true
                                            horizontalAlignment: Text.AlignHCenter
                                            text: modelData
                                            color: "${c "base04"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 10
                                        }
                                    }

                                    Repeater {
                                        // 42 cells = 6 weeks × 7 days
                                        model: 42
                                        Item {
                                            id: cell
                                            Layout.fillWidth: true
                                            Layout.preferredHeight: 18
                                            // Monday-based: getDay() returns 0=Sun..6=Sat
                                            property var firstOfMonth: new Date(calCard.year, calCard.month, 1)
                                            property int firstDow: (firstOfMonth.getDay() + 6) % 7
                                            property int dayNum: index - firstDow + 1
                                            property var cellDate: new Date(calCard.year, calCard.month, dayNum)
                                            property bool inMonth: dayNum >= 1
                                                && cellDate.getMonth() === calCard.month
                                            property bool isToday: inMonth
                                                && cellDate.getDate() === calCard.today.getDate()

                                            Rectangle {
                                                anchors.centerIn: parent
                                                width: 22; height: 18
                                                radius: 4
                                                color: cell.isToday ? "${c "base0D"}" : "transparent"
                                                Text {
                                                    anchors.centerIn: parent
                                                    text: cell.inMonth ? cell.dayNum : ""
                                                    color: cell.isToday
                                                        ? "${c "base00"}"
                                                        : "${c "base05"}"
                                                    font.family: "RobotoMono Nerd Font"
                                                    font.pixelSize: 10
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
        }
      '';

      notificationsQml = ''
        import QtQuick
        import QtQuick.Layouts
        import Quickshell
        import Quickshell.Wayland
        import Quickshell.Services.Notifications

        Scope {
            id: root

            // Currently displayed toasts (auto-expire)
            ListModel { id: toasts }

            // Persistent in-session history (shown in Dashboard)
            ListModel { id: history }
            readonly property int maxHistory: 50

            // Public alias so other components (Dashboard) can bind to it
            property alias historyModel: history

            function clearHistory() {
                history.clear()
            }
            function removeHistory(idx) {
                if (idx >= 0 && idx < history.count) history.remove(idx)
            }

            NotificationServer {
                id: server
                keepOnReload: false
                actionsSupported: true
                bodyMarkupSupported: true
                bodySupported: true
                imageSupported: true

                onNotification: function(notif) {
                    notif.tracked = true
                    var entry = {
                        nid: notif.id,
                        summary: notif.summary || "",
                        body: notif.body || "",
                        appName: notif.appName || "",
                        image: notif.image || "",
                        urgency: notif.urgency,
                        time: Qt.formatDateTime(new Date(), "hh:mm")
                    }
                    toasts.append(entry)
                    history.insert(0, entry)
                    while (history.count > root.maxHistory)
                        history.remove(history.count - 1)
                }
            }

            PanelWindow {
                id: stack
                visible: toasts.count > 0

                WlrLayershell.namespace: "quickshell-notifications"
                WlrLayershell.layer: WlrLayer.Overlay
                WlrLayershell.keyboardFocus: WlrKeyboardFocus.None

                anchors {
                    top: true
                    right: true
                }
                margins {
                    top: 50
                    right: 8
                }

                implicitWidth: 420
                implicitHeight: stackCol.implicitHeight + 8
                color: "transparent"

                ColumnLayout {
                    id: stackCol
                    anchors.fill: parent
                    anchors.margins: 4
                    spacing: 10

                    Repeater {
                        model: toasts

                        Rectangle {
                            id: toast
                            required property int index
                            required property int nid
                            required property string summary
                            required property string body
                            required property string appName
                            required property string image
                            required property int urgency

                            Layout.fillWidth: true
                            Layout.preferredHeight: toastCol.implicitHeight + 24
                            color: "${ca "base00" "ee"}"
                            radius: 12
                            border.width: 1
                            border.color: toast.urgency === NotificationUrgency.Critical
                                ? "${c "base08"}"
                                : "${c "base02"}"

                            opacity: 0
                            Component.onCompleted: opacity = 1
                            Behavior on opacity { NumberAnimation { duration: 220 } }

                            Timer {
                                running: toast.urgency !== NotificationUrgency.Critical
                                interval: 6000
                                onTriggered: toasts.remove(toast.index)
                            }

                            ColumnLayout {
                                id: toastCol
                                anchors.fill: parent
                                anchors.margins: 12
                                spacing: 4

                                RowLayout {
                                    Layout.fillWidth: true
                                    spacing: 10

                                    Image {
                                        visible: toast.image !== ""
                                        source: toast.image
                                        sourceSize.width: 36
                                        sourceSize.height: 36
                                        Layout.preferredWidth: 36
                                        Layout.preferredHeight: 36
                                        fillMode: Image.PreserveAspectFit
                                    }

                                    ColumnLayout {
                                        Layout.fillWidth: true
                                        spacing: 1
                                        Text {
                                            Layout.fillWidth: true
                                            text: toast.appName
                                            color: "${c "base0D"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 11
                                            font.weight: Font.Medium
                                            elide: Text.ElideRight
                                        }
                                        Text {
                                            Layout.fillWidth: true
                                            text: toast.summary
                                            color: "${c "base05"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 15
                                            font.weight: Font.Medium
                                            elide: Text.ElideRight
                                        }
                                    }

                                    Text {
                                        text: "󰅖"
                                        color: "${c "base04"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 16
                                        MouseArea {
                                            anchors.fill: parent
                                            cursorShape: Qt.PointingHandCursor
                                            onClicked: toasts.remove(toast.index)
                                        }
                                    }
                                }

                                Text {
                                    Layout.fillWidth: true
                                    visible: toast.body !== ""
                                    text: toast.body
                                    color: "${c "base06"}"
                                    linkColor: "${c "base0C"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 13
                                    wrapMode: Text.Wrap
                                    textFormat: Text.RichText
                                    maximumLineCount: 6
                                    elide: Text.ElideRight
                                    onLinkActivated: function(link) { Qt.openUrlExternally(link) }
                                    HoverHandler {
                                        cursorShape: parent.hoveredLink !== ""
                                            ? Qt.PointingHandCursor
                                            : Qt.ArrowCursor
                                    }
                                }
                            }

                            MouseArea {
                                anchors.fill: parent
                                acceptedButtons: Qt.RightButton
                                onClicked: toasts.remove(toast.index)
                                propagateComposedEvents: true
                            }
                        }
                    }
                }
            }
        }
      '';

      launcherQml = ''
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

            // Pull desktop entries from Quickshell, sort by name, filter by query.
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

                    transformOrigin: Item.TopLeft
                    opacity: root.opened ? 1 : 0
                    scale: root.opened ? 1 : 0.96
                    Behavior on opacity {
                        NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
                    }
                    Behavior on scale {
                        NumberAnimation { duration: 220; easing.type: Easing.OutCubic }
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
      '';

      sessionQml = ''
        import QtQuick
        import QtQuick.Layouts
        import Quickshell
        import Quickshell.Wayland
        import Quickshell.Io

        Scope {
            id: root

            // Optional reference to the lock component, set by shell.qml.
            property var lockComponent: null

            property bool opened: false

            function show()   { root.opened = true }
            function hide()   { root.opened = false }
            function toggle() { root.opened = !root.opened }

            IpcHandler {
                target: "session"
                function show()   { root.show() }
                function hide()   { root.hide() }
                function toggle() { root.toggle() }
            }

            function run(cmd) {
                Quickshell.execDetached(cmd)
                root.hide()
            }

            function doLock() {
                if (root.lockComponent) root.lockComponent.lock()
                root.hide()
            }
            function doSuspend()  { run(["systemctl", "suspend"]) }
            function doLogout()   { run(["hyprctl", "dispatch", "exit"]) }
            function doReboot()   { run(["systemctl", "reboot"]) }
            function doShutdown() { run(["systemctl", "poweroff"]) }

            PanelWindow {
                id: panel
                visible: root.opened || sessionCard.opacity > 0.01

                WlrLayershell.namespace: "quickshell-session"
                WlrLayershell.layer: WlrLayer.Overlay
                WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

                anchors {
                    top: true
                    right: true
                }
                margins {
                    top: 48
                    right: 8
                }

                implicitWidth: 280
                implicitHeight: contentCol.implicitHeight + 28
                color: "transparent"

                Shortcut {
                    sequences: ["Escape"]
                    onActivated: root.hide()
                }

                Rectangle {
                    id: sessionCard
                    anchors.fill: parent
                    color: "${ca "base00" "ee"}"
                    radius: 16
                    border.width: 1
                    border.color: "${c "base02"}"

                    transformOrigin: Item.TopRight
                    opacity: root.opened ? 1 : 0
                    scale: root.opened ? 1 : 0.96
                    Behavior on opacity {
                        NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
                    }
                    Behavior on scale {
                        NumberAnimation { duration: 220; easing.type: Easing.OutCubic }
                    }

                    ColumnLayout {
                        id: contentCol
                        anchors.fill: parent
                        anchors.margins: 14
                        spacing: 8

                        Text {
                            Layout.alignment: Qt.AlignHCenter
                            text: "Session"
                            color: "${c "base0D"}"
                            font.family: "RobotoMono Nerd Font"
                            font.pixelSize: 14
                            font.weight: Font.Medium
                        }

                        Item { Layout.preferredHeight: 4 }

                        // Helper component: each row is a styled button.
                        Repeater {
                            model: [
                                { icon: "󰌾", label: "Lock",     color: "${c "base0B"}", action: "lock"     },
                                { icon: "󰒲", label: "Suspend",  color: "${c "base0C"}", action: "suspend"  },
                                { icon: "󰍃", label: "Log out",  color: "${c "base0A"}", action: "logout"   },
                                { icon: "󰜉", label: "Reboot",   color: "${c "base09"}", action: "reboot"   },
                                { icon: "󰐥", label: "Shutdown", color: "${c "base08"}", action: "shutdown" }
                            ]

                            Rectangle {
                                required property var modelData
                                Layout.fillWidth: true
                                Layout.preferredHeight: 44
                                radius: 10
                                color: hover.hovered
                                    ? "${ca "base02" "cc"}"
                                    : "${ca "base01" "75"}"
                                border.width: 1
                                border.color: "${c "base02"}"

                                Behavior on color {
                                    ColorAnimation { duration: 120 }
                                }

                                HoverHandler { id: hover }

                                RowLayout {
                                    anchors.fill: parent
                                    anchors.leftMargin: 14
                                    anchors.rightMargin: 14
                                    spacing: 14

                                    Text {
                                        text: modelData.icon
                                        color: modelData.color
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 20
                                    }

                                    Text {
                                        Layout.fillWidth: true
                                        text: modelData.label
                                        color: "${c "base05"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 13
                                        font.weight: Font.Medium
                                    }
                                }

                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: {
                                        switch (modelData.action) {
                                            case "lock":     root.doLock();     break
                                            case "suspend":  root.doSuspend();  break
                                            case "logout":   root.doLogout();   break
                                            case "reboot":   root.doReboot();   break
                                            case "shutdown": root.doShutdown(); break
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
      '';

      lockQml = ''
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
                            // Most password prompts are PromptEchoOff
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
      '';

      bivrostConfig = pkgs.runCommand "quickshell-bivrost" { } ''
        mkdir -p $out
        cp ${pkgs.writeText "shell.qml" shellQml}                 $out/shell.qml
        cp ${pkgs.writeText "Bar.qml" barQml}                     $out/Bar.qml
        cp ${pkgs.writeText "Dashboard.qml" dashboardQml}         $out/Dashboard.qml
        cp ${pkgs.writeText "Notifications.qml" notificationsQml} $out/Notifications.qml
        cp ${pkgs.writeText "Launcher.qml" launcherQml}           $out/Launcher.qml
        cp ${pkgs.writeText "Session.qml" sessionQml}             $out/Session.qml
        cp ${pkgs.writeText "Lock.qml" lockQml}                   $out/Lock.qml
      '';
    in
    {
      programs.quickshell = lib.mkIf (osConfig.environment.desktop.windowManager == "hyprland") {
        enable = true;
        package = pkgs.quickshell;

        configs.bivrost = bivrostConfig;
        activeConfig = "bivrost";

        systemd = {
          enable = true;
          target = "graphical-session.target";
        };
      };
    };
}
