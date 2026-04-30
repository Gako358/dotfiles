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
            SystemMonitor { id: sysmon }
            VolumePanel   { id: volumePanel }
            NetworkPanel  { id: networkPanel }

            function showOnly(which) {
                if (which !== "sysmon")    sysmon.hide()
                if (which !== "volume")    volumePanel.hide()
                if (which !== "network")   networkPanel.hide()
                if (which !== "dashboard") dashboard.hide()
                if (which !== "session")   session.hide()
                if (which !== "launcher")  launcher.hide()
            }

            Variants {
                model: Quickshell.screens

                Bar {
                    required property var modelData
                    screen: modelData
                    onLauncherRequested:      { root.showOnly("launcher");  launcher.toggle() }
                    onDashboardRequested:     { root.showOnly("dashboard"); dashboard.toggle() }
                    onCalendarRequested:      { root.showOnly("dashboard"); dashboard.show() }
                    onSessionRequested:       { root.showOnly("session");   session.toggle() }
                    onSystemMonitorRequested: { root.showOnly("sysmon");    sysmon.toggle() }
                    onAudioRequested:         { root.showOnly("volume");    volumePanel.toggle() }
                    onNetworkRequested:       { root.showOnly("network");   networkPanel.toggle() }
                }
            }
        }
      '';

      barQml = ''
        import QtQuick
        import QtQuick.Layouts
        import Quickshell
        import Quickshell.Hyprland
        import Quickshell.Io
        import Quickshell.Services.Pipewire
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

            property string cpuPct: "—"
            property string netState: "off"
            property int    netSignal: 0
            property string netSsid: ""

            Process {
                id: barCpuProc
                command: ["sh", "-c",
                    "top -bn1 | awk '/Cpu/ { printf \"%.0f\", 100 - $8 }'"]
                stdout: StdioCollector { id: barCpuOut }
                onExited: bar.cpuPct = ((barCpuOut.text || "").trim() || "0")
            }

            Process {
                id: barNetProc
                command: ["sh", "-c",
                    "act=$(nmcli -t -f TYPE,STATE,CONNECTION device status 2>/dev/null | awk -F: '$2==\"connected\"{print $1\":\"$3; exit}'); " +
                    "if echo \"$act\" | grep -q '^wifi:'; then " +
                    "  ssid=$(echo \"$act\" | cut -d: -f2-); " +
                    "  sig=$(nmcli -t -f IN-USE,SIGNAL device wifi 2>/dev/null | awk -F: '$1==\"*\"{print $2; exit}'); " +
                    "  echo \"wifi|$sig|$ssid\"; " +
                    "elif echo \"$act\" | grep -q '^ethernet:'; then " +
                    "  echo \"wired|100|$(echo \"$act\" | cut -d: -f2-)\"; " +
                    "else echo \"off|0|\"; fi"]
                stdout: StdioCollector { id: barNetOut }
                onExited: {
                    var parts = ((barNetOut.text || "").trim() || "off|0|").split("|")
                    bar.netState  = parts[0] || "off"
                    bar.netSignal = parseInt(parts[1] || "0") || 0
                    bar.netSsid   = parts[2] || ""
                }
            }

            Timer {
                running: true
                repeat: true
                interval: 3000
                triggeredOnStart: true
                onTriggered: {
                    barCpuProc.running = true
                    barNetProc.running = true
                }
            }

            PwObjectTracker {
                objects: Pipewire.defaultAudioSink ? [Pipewire.defaultAudioSink] : []
            }

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
                        spacing: 4
                        Repeater {
                            model: 9
                            Item {
                                id: ws
                                property int wsId: index + 1
                                property bool isActive: Hyprland.focusedWorkspace
                                    && Hyprland.focusedWorkspace.id === wsId
                                property bool hasIcon: wsId === 1 || wsId === 2
                                    || wsId === 3 || wsId === 5
                                    || wsId === 8 || wsId === 9
                                property bool wsExists: {
                                    var list = Hyprland.workspaces
                                        ? Hyprland.workspaces.values
                                        : []
                                    for (var i = 0; i < list.length; ++i) {
                                        if (list[i] && list[i].id === ws.wsId)
                                            return true
                                    }
                                    return false
                                }
                                property string icon: {
                                    switch (wsId) {
                                        case 1: return "󰈹"   // browsing  (nf-md-firefox)
                                        case 2: return "󰅴"   // coding    (nf-md-emacs)
                                        case 3: return "󰆍"   // terminal  (nf-md-console)
                                        case 5: return "󰢹"   // qemu / vm (nf-md-monitor)
                                        case 8: return "󰒱"   // slack     (nf-md-slack)
                                        case 9: return "󰙯"   // discord   (nf-md-discord)
                                        default: return wsId.toString()
                                    }
                                }

                                visible: hasIcon || wsExists || isActive

                                width: isActive ? 34 : 22
                                height: 22

                                Behavior on width {
                                    NumberAnimation { duration: 240; easing.type: Easing.OutQuint }
                                }

                                Rectangle {
                                    anchors.fill: parent
                                    radius: height / 2
                                    color: ws.isActive
                                        ? "${c "base0D"}"
                                        : (wsHover.hovered
                                            ? "${ca "base02" "cc"}"
                                            : "transparent")
                                    Behavior on color {
                                        ColorAnimation { duration: 180 }
                                    }

                                    HoverHandler { id: wsHover }

                                    Text {
                                        anchors.centerIn: parent
                                        text: ws.icon
                                        color: ws.isActive
                                            ? "${c "base00"}"
                                            : "${c "base04"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: ws.isActive ? 14 : 11
                                        Behavior on font.pixelSize {
                                            NumberAnimation {
                                                duration: 240
                                                easing.type: Easing.OutQuint
                                            }
                                        }
                                        Behavior on color {
                                            ColorAnimation { duration: 180 }
                                        }
                                    }
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

                    // ── Status pill (segmented, caelestia-style) ─────
                    Rectangle {
                        id: statusPill
                        visible: bar.isFocused
                        Layout.alignment: Qt.AlignVCenter
                        Layout.preferredHeight: 26
                        Layout.preferredWidth: statusRow.implicitWidth + 6
                        color: "${ca "base01" "aa"}"
                        border.color: "${c "base02"}"
                        border.width: 1
                        radius: 13

                        readonly property var sink: Pipewire.defaultAudioSink
                        readonly property real volPct:
                            sink && sink.audio ? sink.audio.volume * 100 : 0
                        readonly property bool muted:
                            sink && sink.audio ? sink.audio.muted : true

                        function volIcon(p, m) {
                            if (m || p <= 0) return "󰝟"
                            if (p < 34)      return "󰕿"
                            if (p < 67)      return "󰖀"
                            return "󰕾"
                        }
                        function wifiIcon(s) {
                            if (s >= 75) return "󰤨"
                            if (s >= 50) return "󰤥"
                            if (s >= 25) return "󰤢"
                            if (s > 0)   return "󰤟"
                            return "󰤮"
                        }

                        Row {
                            id: statusRow
                            anchors.centerIn: parent
                            spacing: 0

                            // ── System monitor segment (CPU%) ────────
                            Rectangle {
                                id: cpuSeg
                                width: cpuRow.implicitWidth + 16
                                height: statusPill.height
                                color: cpuHover.hovered
                                    ? "${ca "base02" "cc"}"
                                    : "transparent"
                                radius: 13
                                Behavior on color { ColorAnimation { duration: 120 } }
                                HoverHandler { id: cpuHover }
                                Row {
                                    id: cpuRow
                                    anchors.centerIn: parent
                                    spacing: 6
                                    Text {
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: "󰻠"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 13
                                        color: "${c "base0C"}"
                                    }
                                    Text {
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: bar.cpuPct + "%"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 11
                                        color: "${c "base05"}"
                                    }
                                }
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: bar.systemMonitorRequested()
                                }
                            }

                            Rectangle {
                                width: 1; height: statusPill.height - 10
                                anchors.verticalCenter: parent.verticalCenter
                                color: "${ca "base03" "80"}"
                            }

                            // ── Volume segment ───────────────────────
                            Rectangle {
                                id: volSeg
                                width: volRow.implicitWidth + 16
                                height: statusPill.height
                                color: volHover.hovered
                                    ? "${ca "base02" "cc"}"
                                    : "transparent"
                                Behavior on color { ColorAnimation { duration: 120 } }
                                HoverHandler { id: volHover }
                                Row {
                                    id: volRow
                                    anchors.centerIn: parent
                                    spacing: 6
                                    Text {
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: statusPill.volIcon(statusPill.volPct, statusPill.muted)
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 13
                                        color: statusPill.muted
                                            ? "${c "base08"}"
                                            : "${c "base0E"}"
                                    }
                                    Text {
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: statusPill.muted
                                            ? "muted"
                                            : Math.round(statusPill.volPct) + "%"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 11
                                        color: "${c "base05"}"
                                    }
                                }
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton
                                    onClicked: function(mouse) {
                                        if (mouse.button === Qt.MiddleButton) {
                                            if (statusPill.sink && statusPill.sink.audio)
                                                statusPill.sink.audio.muted = !statusPill.sink.audio.muted
                                        } else {
                                            bar.audioRequested()
                                        }
                                    }
                                    onWheel: function(wheel) {
                                        if (!statusPill.sink || !statusPill.sink.audio) return
                                        var step = wheel.angleDelta.y > 0 ? 0.05 : -0.05
                                        var v = Math.max(0, Math.min(1,
                                            statusPill.sink.audio.volume + step))
                                        statusPill.sink.audio.volume = v
                                    }
                                }
                            }

                            Rectangle {
                                width: 1; height: statusPill.height - 10
                                anchors.verticalCenter: parent.verticalCenter
                                color: "${ca "base03" "80"}"
                            }

                            // ── Network segment ──────────────────────
                            Rectangle {
                                id: netSeg
                                width: netRow.implicitWidth + 16
                                height: statusPill.height
                                color: netHover.hovered
                                    ? "${ca "base02" "cc"}"
                                    : "transparent"
                                radius: 13
                                Behavior on color { ColorAnimation { duration: 120 } }
                                HoverHandler { id: netHover }
                                Row {
                                    id: netRow
                                    anchors.centerIn: parent
                                    spacing: 6
                                    Text {
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: bar.netState === "wired"
                                            ? "󰈀"
                                            : statusPill.wifiIcon(bar.netSignal)
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 13
                                        color: bar.netState === "off"
                                            ? "${c "base08"}"
                                            : "${c "base0B"}"
                                    }
                                    Text {
                                        visible: text !== ""
                                        anchors.verticalCenter: parent.verticalCenter
                                        text: bar.netState === "off"
                                            ? "offline"
                                            : (bar.netState === "wired"
                                                ? "wired"
                                                : (bar.netSsid.length > 12
                                                    ? bar.netSsid.substring(0, 12) + "…"
                                                    : bar.netSsid))
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 11
                                        color: "${c "base05"}"
                                        elide: Text.ElideRight
                                    }
                                }
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

                    property real slideY: root.opened ? 0 : -18
                    transform: Translate { y: dashCard.slideY }
                    transformOrigin: Item.TopRight
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
                                        model: 42
                                        Item {
                                            id: cell
                                            Layout.fillWidth: true
                                            Layout.preferredHeight: 18
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

            ListModel { id: toasts }
            ListModel { id: history }
            readonly property int maxHistory: 50

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
      '';

      sessionQml = ''
        import QtQuick
        import QtQuick.Layouts
        import Quickshell
        import Quickshell.Wayland
        import Quickshell.Io

        Scope {
            id: root

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

                    property real slideY: root.opened ? 0 : -18
                    transform: Translate { y: sessionCard.slideY }

                    transformOrigin: Item.TopRight
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

      sysmonQml = ''
        import QtQuick
        import QtQuick.Controls
        import QtQuick.Layouts
        import Quickshell
        import Quickshell.Wayland
        import Quickshell.Io

        Scope {
            id: root

            property bool opened: false
            function show()   { root.opened = true }
            function hide()   { root.opened = false }
            function toggle() { root.opened = !root.opened }

            IpcHandler {
                target: "sysmon"
                function show()   { root.show() }
                function hide()   { root.hide() }
                function toggle() { root.toggle() }
            }

            property real cpuPct: 0
            property real memPct: 0
            property string memInfo: "—"
            property real swapPct: 0
            property string swapInfo: "—"
            property string uptime: "—"
            property var procs: []

            Process {
                id: cpuProc
                command: ["sh", "-c",
                    "top -bn1 | awk '/Cpu/ { printf \"%.0f\", 100 - $8 }'"]
                stdout: StdioCollector { id: cpuOut }
                onExited: root.cpuPct =
                    parseFloat((cpuOut.text || "").trim() || "0") || 0
            }
            Process {
                id: memProc
                command: ["sh", "-c",
                    "free -b | awk '" +
                    "/^Mem:/  { printf \"%.1f|%.1f|%.1f\", $3/1073741824, $2/1073741824, ($3/$2)*100 } " +
                    "/^Swap:/ { if ($2>0) printf \"|%.1f|%.1f|%.1f\", $3/1073741824, $2/1073741824, ($3/$2)*100; else printf \"|0|0|0\" }'"]
                stdout: StdioCollector { id: memOut }
                onExited: {
                    var p = ((memOut.text || "").trim()).split("|")
                    if (p.length >= 6) {
                        root.memInfo  = p[0] + " / " + p[1] + " GiB"
                        root.memPct   = parseFloat(p[2]) || 0
                        root.swapInfo = p[3] + " / " + p[4] + " GiB"
                        root.swapPct  = parseFloat(p[5]) || 0
                    }
                }
            }
            Process {
                id: upProc
                command: ["sh", "-c",
                    "awk '{u=int($1); d=int(u/86400); h=int((u%86400)/3600); m=int((u%3600)/60); s=\"\"; if(d>0) s=s d\"d \"; if(h>0) s=s h\"h \"; s=s m\"m\"; print s}' /proc/uptime"]
                stdout: StdioCollector { id: upOut }
                onExited: root.uptime = (upOut.text || "").trim() || "—"
            }
            Process {
                id: psProc
                command: ["sh", "-c",
                    "ps -eo comm,%cpu,%mem --sort=-%cpu --no-headers | head -n 5"]
                stdout: StdioCollector { id: psOut }
                onExited: {
                    var lines = (psOut.text || "").trim().split("\n")
                    var arr = []
                    for (var i = 0; i < lines.length; ++i) {
                        var f = lines[i].trim().split(/\s+/)
                        if (f.length < 3) continue
                        arr.push({
                            name: f[0],
                            cpu:  parseFloat(f[1]) || 0,
                            mem:  parseFloat(f[2]) || 0
                        })
                    }
                    root.procs = arr
                }
            }

            Timer {
                running: root.opened
                repeat: true
                interval: 2000
                triggeredOnStart: true
                onTriggered: {
                    cpuProc.running = true
                    memProc.running = true
                    upProc.running  = true
                    psProc.running  = true
                }
            }

            PanelWindow {
                id: panel
                visible: root.opened || card.opacity > 0.01

                WlrLayershell.namespace: "quickshell-sysmon"
                WlrLayershell.layer: WlrLayer.Overlay
                WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

                anchors { top: true; right: true }
                margins { top: 50; right: 8 }
                implicitWidth: 380
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
                            text: "󰍛  System Monitor"
                            color: "${c "base0D"}"
                            font.family: "RobotoMono Nerd Font"
                            font.pixelSize: 14
                            font.weight: Font.Medium
                        }

                        // ── CPU ──────────────────────────────────────
                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 4
                            RowLayout {
                                Layout.fillWidth: true
                                Text {
                                    text: "󰻠  CPU"
                                    color: "${c "base0C"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 12
                                }
                                Item { Layout.fillWidth: true }
                                Text {
                                    text: Math.round(root.cpuPct) + "%"
                                    color: "${c "base05"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 12
                                }
                            }
                            Rectangle {
                                Layout.fillWidth: true
                                Layout.preferredHeight: 6
                                radius: 3
                                color: "${ca "base02" "80"}"
                                Rectangle {
                                    anchors.left: parent.left
                                    anchors.top: parent.top
                                    anchors.bottom: parent.bottom
                                    width: parent.width * Math.min(1, root.cpuPct / 100)
                                    radius: 3
                                    color: "${c "base0C"}"
                                    Behavior on width {
                                        NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                                    }
                                }
                            }
                        }

                        // ── Memory ───────────────────────────────────
                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 4
                            RowLayout {
                                Layout.fillWidth: true
                                Text {
                                    text: "󰘚  Memory"
                                    color: "${c "base0E"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 12
                                }
                                Item { Layout.fillWidth: true }
                                Text {
                                    text: root.memInfo
                                    color: "${c "base05"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 11
                                }
                            }
                            Rectangle {
                                Layout.fillWidth: true
                                Layout.preferredHeight: 6
                                radius: 3
                                color: "${ca "base02" "80"}"
                                Rectangle {
                                    anchors.left: parent.left
                                    anchors.top: parent.top
                                    anchors.bottom: parent.bottom
                                    width: parent.width * Math.min(1, root.memPct / 100)
                                    radius: 3
                                    color: "${c "base0E"}"
                                    Behavior on width {
                                        NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                                    }
                                }
                            }
                        }

                        // ── Swap ─────────────────────────────────────
                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 4
                            RowLayout {
                                Layout.fillWidth: true
                                Text {
                                    text: "󰓡  Swap"
                                    color: "${c "base09"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 12
                                }
                                Item { Layout.fillWidth: true }
                                Text {
                                    text: root.swapInfo
                                    color: "${c "base05"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 11
                                }
                            }
                            Rectangle {
                                Layout.fillWidth: true
                                Layout.preferredHeight: 6
                                radius: 3
                                color: "${ca "base02" "80"}"
                                Rectangle {
                                    anchors.left: parent.left
                                    anchors.top: parent.top
                                    anchors.bottom: parent.bottom
                                    width: parent.width * Math.min(1, root.swapPct / 100)
                                    radius: 3
                                    color: "${c "base09"}"
                                    Behavior on width {
                                        NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                                    }
                                }
                            }
                        }

                        // ── Uptime ───────────────────────────────────
                        RowLayout {
                            Layout.fillWidth: true
                            Text {
                                text: "󰅐  Uptime"
                                color: "${c "base0A"}"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 12
                            }
                            Item { Layout.fillWidth: true }
                            Text {
                                text: root.uptime
                                color: "${c "base05"}"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 12
                            }
                        }

                        // ── Top processes ────────────────────────────
                        Rectangle {
                            Layout.fillWidth: true
                            Layout.preferredHeight: procsCol.implicitHeight + 16
                            color: "${ca "base01" "75"}"
                            radius: 10
                            border.width: 1
                            border.color: "${c "base02"}"

                            ColumnLayout {
                                id: procsCol
                                anchors.fill: parent
                                anchors.margins: 8
                                spacing: 4

                                Text {
                                    text: "Top processes"
                                    color: "${c "base04"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 10
                                }

                                Repeater {
                                    model: root.procs
                                    RowLayout {
                                        Layout.fillWidth: true
                                        spacing: 8
                                        Text {
                                            Layout.fillWidth: true
                                            text: modelData.name
                                            color: "${c "base05"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 11
                                            elide: Text.ElideRight
                                        }
                                        Text {
                                            text: modelData.cpu.toFixed(1) + "%"
                                            color: "${c "base0C"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 11
                                        }
                                        Text {
                                            text: modelData.mem.toFixed(1) + "%"
                                            color: "${c "base0E"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 11
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

      volumePanelQml = ''
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
      '';

      networkPanelQml = ''
        import QtQuick
        import QtQuick.Controls
        import QtQuick.Layouts
        import Quickshell
        import Quickshell.Wayland
        import Quickshell.Io

        Scope {
            id: root

            property bool opened: false
            function show()   { root.opened = true; root.refresh() }
            function hide()   { root.opened = false }
            function toggle() { if (root.opened) root.hide(); else root.show() }

            IpcHandler {
                target: "network"
                function show()   { root.show() }
                function hide()   { root.hide() }
                function toggle() { root.toggle() }
            }

            property string activeSsid: ""
            property string activeType: "off"
            property bool   wifiEnabled: true
            property var    networks: []
            property bool   busy: false

            function refresh() {
                statusProc.running   = true
                wifiListProc.running = true
            }

            Process {
                id: statusProc
                command: ["sh", "-c",
                    "wifi=$(nmcli -t -f WIFI g 2>/dev/null); " +
                    "act=$(nmcli -t -f TYPE,STATE,CONNECTION device status 2>/dev/null | awk -F: '$2==\"connected\"{print $1\":\"$3; exit}'); " +
                    "echo \"$wifi||$act\""]
                stdout: StdioCollector { id: statusOut }
                onExited: {
                    var raw   = (statusOut.text || "").trim()
                    var parts = raw.split("||")
                    root.wifiEnabled =
                        (parts[0] || "").toLowerCase().indexOf("enabled") !== -1
                    var act = parts[1] || ""
                    if (act.indexOf("wifi:") === 0) {
                        root.activeType = "wifi"
                        root.activeSsid = act.substring(5)
                    } else if (act.indexOf("ethernet:") === 0) {
                        root.activeType = "wired"
                        root.activeSsid = act.substring(9)
                    } else {
                        root.activeType = "off"
                        root.activeSsid = ""
                    }
                }
            }

            Process {
                id: wifiListProc
                command: ["sh", "-c",
                    "nmcli -t -f IN-USE,SSID,SIGNAL,SECURITY device wifi list 2>/dev/null | head -n 30"]
                stdout: StdioCollector { id: wifiListOut }
                onExited: {
                    var lines = (wifiListOut.text || "").trim().split("\n")
                    var arr = []
                    for (var i = 0; i < lines.length; ++i) {
                        var raw = lines[i]
                        if (!raw) continue
                        var safe  = raw.replace(/\\:/g, "\u0001")
                        var parts = safe.split(":")
                        if (parts.length < 4) continue
                        var ssid = (parts[1] || "").replace(/\u0001/g, ":")
                        if (ssid === "") continue
                        arr.push({
                            inUse:  (parts[0] || "") === "*",
                            ssid:   ssid,
                            signal: parseInt(parts[2] || "0") || 0,
                            secure: (parts[3] || "") !== ""
                        })
                    }
                    arr.sort(function(a, b) { return b.signal - a.signal })
                    var seen = ({})
                    var unique = []
                    for (var j = 0; j < arr.length; ++j) {
                        if (seen[arr[j].ssid]) continue
                        seen[arr[j].ssid] = true
                        unique.push(arr[j])
                    }
                    root.networks = unique
                }
            }

            Process {
                id: actionProc
                onExited: {
                    root.busy = false
                    root.refresh()
                }
            }

            function toggleWifi() {
                root.busy = true
                actionProc.command = ["sh", "-c",
                    "nmcli radio wifi " + (root.wifiEnabled ? "off" : "on")]
                actionProc.running = true
            }
            function connectTo(ssid) {
                root.busy = true
                actionProc.command = ["sh", "-c",
                    "nmcli device wifi connect " + JSON.stringify(ssid) + " || true"]
                actionProc.running = true
            }
            function disconnectActive() {
                if (root.activeSsid === "") return
                root.busy = true
                actionProc.command = ["sh", "-c",
                    "nmcli connection down id " + JSON.stringify(root.activeSsid) + " || true"]
                actionProc.running = true
            }

            Timer {
                running: root.opened
                repeat: true
                interval: 5000
                onTriggered: root.refresh()
            }

            function wifiIcon(s) {
                if (s >= 75) return "󰤨"
                if (s >= 50) return "󰤥"
                if (s >= 25) return "󰤢"
                if (s > 0)   return "󰤟"
                return "󰤮"
            }

            PanelWindow {
                id: panel
                visible: root.opened || card.opacity > 0.01

                WlrLayershell.namespace: "quickshell-network"
                WlrLayershell.layer: WlrLayer.Overlay
                WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

                anchors { top: true; right: true }
                margins { top: 50; right: 8 }
                implicitWidth: 380
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
                        anchors.fill: parent
                        anchors.margins: 14
                        spacing: 10

                        RowLayout {
                            Layout.fillWidth: true
                            Text {
                                text: "󰖩  Network"
                                color: "${c "base0B"}"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 14
                                font.weight: Font.Medium
                            }
                            Item { Layout.fillWidth: true }
                            Text {
                                text: root.busy ? "…" : "󰑐"
                                color: "${c "base04"}"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 14
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: root.refresh()
                                }
                            }
                        }

                        // ── Status / current connection ──────────────
                        Rectangle {
                            Layout.fillWidth: true
                            Layout.preferredHeight: 56
                            color: "${ca "base01" "75"}"
                            radius: 10
                            border.width: 1
                            border.color: "${c "base02"}"

                            RowLayout {
                                anchors.fill: parent
                                anchors.margins: 12
                                spacing: 10

                                Text {
                                    text: root.activeType === "wired"
                                        ? "󰈀"
                                        : (root.activeType === "wifi"
                                            ? root.wifiIcon(80)
                                            : "󰤮")
                                    color: root.activeType === "off"
                                        ? "${c "base08"}"
                                        : "${c "base0B"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 22
                                }

                                ColumnLayout {
                                    Layout.fillWidth: true
                                    spacing: 0
                                    Text {
                                        text: root.activeType === "off"
                                            ? "Disconnected"
                                            : (root.activeType === "wired"
                                                ? "Wired"
                                                : "Connected")
                                        color: "${c "base04"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 10
                                    }
                                    Text {
                                        Layout.fillWidth: true
                                        text: root.activeSsid !== "" ? root.activeSsid : "—"
                                        color: "${c "base05"}"
                                        font.family: "RobotoMono Nerd Font"
                                        font.pixelSize: 12
                                        elide: Text.ElideRight
                                    }
                                }

                                Text {
                                    visible: root.activeType !== "off"
                                    text: "Disconnect"
                                    color: "${c "base08"}"
                                    font.family: "RobotoMono Nerd Font"
                                    font.pixelSize: 10
                                    MouseArea {
                                        anchors.fill: parent
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: root.disconnectActive()
                                    }
                                }
                            }
                        }

                        // ── Wi-Fi toggle ─────────────────────────────
                        RowLayout {
                            Layout.fillWidth: true
                            Text {
                                text: "Wi-Fi"
                                color: "${c "base05"}"
                                font.family: "RobotoMono Nerd Font"
                                font.pixelSize: 12
                            }
                            Item { Layout.fillWidth: true }
                            Rectangle {
                                Layout.preferredWidth: 38
                                Layout.preferredHeight: 20
                                radius: 10
                                color: root.wifiEnabled
                                    ? "${c "base0D"}"
                                    : "${ca "base02" "cc"}"
                                Behavior on color { ColorAnimation { duration: 180 } }

                                Rectangle {
                                    width: 16; height: 16; radius: 8
                                    color: "${c "base05"}"
                                    anchors.verticalCenter: parent.verticalCenter
                                    x: root.wifiEnabled
                                        ? parent.width - width - 2
                                        : 2
                                    Behavior on x {
                                        NumberAnimation { duration: 200; easing.type: Easing.OutCubic }
                                    }
                                }
                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: root.toggleWifi()
                                }
                            }
                        }

                        // ── Network list ─────────────────────────────
                        Rectangle {
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            color: "${ca "base01" "75"}"
                            radius: 10
                            border.width: 1
                            border.color: "${c "base02"}"

                            ListView {
                                id: netList
                                anchors.fill: parent
                                anchors.margins: 6
                                clip: true
                                spacing: 2
                                model: root.networks
                                boundsBehavior: Flickable.StopAtBounds
                                ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded }

                                delegate: Rectangle {
                                    required property var modelData
                                    width: netList.width
                                    height: 38
                                    radius: 6
                                    color: hover.hovered
                                        ? "${ca "base02" "aa"}"
                                        : "transparent"
                                    Behavior on color { ColorAnimation { duration: 120 } }
                                    HoverHandler { id: hover }

                                    RowLayout {
                                        anchors.fill: parent
                                        anchors.leftMargin: 10
                                        anchors.rightMargin: 10
                                        spacing: 10

                                        Text {
                                            text: root.wifiIcon(modelData.signal)
                                            color: modelData.inUse
                                                ? "${c "base0B"}"
                                                : "${c "base05"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 16
                                        }
                                        Text {
                                            Layout.fillWidth: true
                                            text: modelData.ssid
                                            color: "${c "base05"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 12
                                            elide: Text.ElideRight
                                            font.weight: modelData.inUse
                                                ? Font.Medium
                                                : Font.Normal
                                        }
                                        Text {
                                            visible: modelData.secure
                                            text: "󰌾"
                                            color: "${c "base04"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 11
                                        }
                                        Text {
                                            text: modelData.signal + "%"
                                            color: "${c "base04"}"
                                            font.family: "RobotoMono Nerd Font"
                                            font.pixelSize: 10
                                        }
                                    }

                                    MouseArea {
                                        anchors.fill: parent
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: {
                                            if (!modelData.inUse)
                                                root.connectTo(modelData.ssid)
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

      bivrostConfig = pkgs.runCommand "quickshell-bivrost" { } ''
        mkdir -p $out
        cp ${pkgs.writeText "shell.qml" shellQml}                 $out/shell.qml
        cp ${pkgs.writeText "Bar.qml" barQml}                     $out/Bar.qml
        cp ${pkgs.writeText "Dashboard.qml" dashboardQml}         $out/Dashboard.qml
        cp ${pkgs.writeText "Notifications.qml" notificationsQml} $out/Notifications.qml
        cp ${pkgs.writeText "Launcher.qml" launcherQml}           $out/Launcher.qml
        cp ${pkgs.writeText "Session.qml" sessionQml}             $out/Session.qml
        cp ${pkgs.writeText "Lock.qml" lockQml}                   $out/Lock.qml
        cp ${pkgs.writeText "SystemMonitor.qml" sysmonQml}        $out/SystemMonitor.qml
        cp ${pkgs.writeText "VolumePanel.qml" volumePanelQml}     $out/VolumePanel.qml
        cp ${pkgs.writeText "NetworkPanel.qml" networkPanelQml}   $out/NetworkPanel.qml
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
