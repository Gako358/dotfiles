''
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
      TrayPanel     { id: trayPanel }
      Wallpaper     { id: wallpaper }

      function showOnly(which) {
          if (which !== "sysmon")    sysmon.hide()
          if (which !== "volume")    volumePanel.hide()
          if (which !== "network")   networkPanel.hide()
          if (which !== "tray")      trayPanel.hide()
          if (which !== "dashboard") dashboard.hide()
          if (which !== "session")   session.hide()
          if (which !== "launcher")  launcher.hide()
          if (which !== "wallpaper") wallpaper.hide()
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
              onTrayRequested:          { root.showOnly("tray");      trayPanel.toggle() }
              onWallpaperRequested:     { root.showOnly("wallpaper"); wallpaper.toggle() }
          }
      }
  }
''
