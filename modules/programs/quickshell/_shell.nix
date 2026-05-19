{
  lib,
  battery ? false,
}:
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
          onMonitorsRequested:  { root.showOnly("monitors");  monitorsPanel.show() }
          onWallpaperRequested: { root.showOnly("wallpaper"); wallpaper.toggle() }
          onSessionRequested:   { root.showOnly("session");   session.toggle() }
          onAudioRequested:     { root.showOnly("volume");    volumePanel.toggle() }
          onNetworkRequested:   { root.showOnly("network");   networkPanel.toggle() }
          ${lib.optionalString battery ''
            onBatteryRequested:   { root.showOnly("battery");   batteryPanel.toggle() }
          ''}
          onProcessesRequested: function(sortMode) {
              root.showOnly("processes")
              processesPanel.sortMode = sortMode
              processesPanel.show()
          }
      }
      SystemMonitor    { id: sysmon }
      VolumePanel      { id: volumePanel }
      NetworkPanel     { id: networkPanel }
      MonitorsPanel    { id: monitorsPanel }
      TrayPanel        { id: trayPanel }
      Wallpaper        { id: wallpaper }
      ProcessesPanel   { id: processesPanel }
      ${lib.optionalString battery ''
        BatteryPanel     { id: batteryPanel }
      ''}

      function showOnly(which) {
          if (which !== "sysmon")     sysmon.hide()
          if (which !== "volume")     volumePanel.hide()
          if (which !== "network")    networkPanel.hide()
          if (which !== "monitors")   monitorsPanel.hide()
          if (which !== "tray")       trayPanel.hide()
          if (which !== "dashboard")  dashboard.hide()
          if (which !== "session")    session.hide()
          if (which !== "launcher")   launcher.hide()
          if (which !== "wallpaper")  wallpaper.hide()
          if (which !== "processes")  processesPanel.hide()
          ${lib.optionalString battery ''
            if (which !== "battery")    batteryPanel.hide()
          ''}
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
