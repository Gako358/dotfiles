_: ''
  import QtQuick
  import Quickshell
  import Quickshell.Io

  Scope {
      id: root

      // Bumped after any mutation. Bind to this in QML for live updates,
      // since ListModel.get() does not auto-track in bindings.
      property int revision: 0

      readonly property string storePath:
          Qt.resolvedUrl("file://" + Quickshell.env("HOME")
              + "/.local/state/quickshell/appointments.json")
              .toString().replace("file://", "")

      ListModel { id: appts }

      // ── Public helpers ────────────────────────────────────────
      function _bump()  { root.revision = root.revision + 1 }

      function _genId() {
          return "a-" + Date.now().toString(36)
              + "-" + Math.floor(Math.random() * 1e6).toString(36)
      }

      function _normalize(item) {
          return {
              id:         item.id || root._genId(),
              title:      String(item.title || "").trim() || "Reminder",
              body:       String(item.body || ""),
              date:       String(item.date || ""),
              time:       String(item.time || "09:00"),
              leadMins:   parseInt(item.leadMins) || 0,
              leadFired:  !!item.leadFired,
              alarmFired: !!item.alarmFired
          }
      }

      function add(item) {
          var e = root._normalize(item)
          if (!e.id) e.id = root._genId()
          appts.append(e)
          root._bump()
          root.save()
          return e.id
      }

      function update(id, patch) {
          for (var i = 0; i < appts.count; i++) {
              if (appts.get(i).id === id) {
                  var cur = appts.get(i)
                  var merged = root._normalize({
                      id:         cur.id,
                      title:      patch.title      !== undefined ? patch.title      : cur.title,
                      body:       patch.body       !== undefined ? patch.body       : cur.body,
                      date:       patch.date       !== undefined ? patch.date       : cur.date,
                      time:       patch.time       !== undefined ? patch.time       : cur.time,
                      leadMins:   patch.leadMins   !== undefined ? patch.leadMins   : cur.leadMins,
                      // If date/time/leadMins changed, reset fired flags so
                      // the alarm can re-trigger.
                      leadFired:  (patch.date !== undefined
                                || patch.time !== undefined
                                || patch.leadMins !== undefined)
                                  ? false
                                  : (patch.leadFired !== undefined ? patch.leadFired : cur.leadFired),
                      alarmFired: (patch.date !== undefined
                                || patch.time !== undefined)
                                  ? false
                                  : (patch.alarmFired !== undefined ? patch.alarmFired : cur.alarmFired)
                  })
                  appts.set(i, merged)
                  root._bump()
                  root.save()
                  return true
              }
          }
          return false
      }

      function remove(id) {
          for (var i = 0; i < appts.count; i++) {
              if (appts.get(i).id === id) {
                  appts.remove(i)
                  root._bump()
                  root.save()
                  return true
              }
          }
          return false
      }

      function clearAll() {
          appts.clear()
          root._bump()
          root.save()
      }

      function get(id) {
          for (var i = 0; i < appts.count; i++)
              if (appts.get(i).id === id) {
                  var x = appts.get(i)
                  // Return a plain JS object copy.
                  return {
                      id: x.id, title: x.title, body: x.body,
                      date: x.date, time: x.time, leadMins: x.leadMins,
                      leadFired: x.leadFired, alarmFired: x.alarmFired
                  }
              }
          return null
      }

      function hasOn(dateStr) {
          if (!dateStr) return false
          for (var i = 0; i < appts.count; i++)
              if (appts.get(i).date === dateStr) return true
          return false
      }

      function countOn(dateStr) {
          if (!dateStr) return 0
          var n = 0
          for (var i = 0; i < appts.count; i++)
              if (appts.get(i).date === dateStr) n++
          return n
      }

      function listForDate(dateStr) {
          var out = []
          if (!dateStr) return out
          for (var i = 0; i < appts.count; i++) {
              var e = appts.get(i)
              if (e.date === dateStr)
                  out.push({
                      id: e.id, title: e.title, body: e.body,
                      date: e.date, time: e.time, leadMins: e.leadMins,
                      leadFired: e.leadFired, alarmFired: e.alarmFired
                  })
          }
          out.sort(function(a, b) {
              return a.time < b.time ? -1 : (a.time > b.time ? 1 : 0)
          })
          return out
      }

      function listInMonth(year, month) {
          // month is 0-based to match JS Date.
          var prefix = year.toString().padStart(4, "0") + "-"
              + (month + 1).toString().padStart(2, "0") + "-"
          var n = 0
          for (var i = 0; i < appts.count; i++)
              if (appts.get(i).date.indexOf(prefix) === 0) n++
          return n
      }

      function nextUpcoming(limit) {
          var out = []
          var nowMs = Date.now()
          for (var i = 0; i < appts.count; i++) {
              var e = appts.get(i)
              if (root._scheduledMs(e) >= nowMs - 60000)
                  out.push({
                      id: e.id, title: e.title, body: e.body,
                      date: e.date, time: e.time, leadMins: e.leadMins,
                      leadFired: e.leadFired, alarmFired: e.alarmFired
                  })
          }
          out.sort(function(a, b) {
              return root._scheduledMs(a) - root._scheduledMs(b)
          })
          return out.slice(0, limit || 5)
      }

      function _scheduledMs(item) {
          var d = (item.date || "").split("-")
          var t = (item.time || "00:00").split(":")
          if (d.length < 3 || t.length < 2) return 0
          var dt = new Date(
              parseInt(d[0]),
              parseInt(d[1]) - 1,
              parseInt(d[2]),
              parseInt(t[0]) || 0,
              parseInt(t[1]) || 0,
              0)
          return dt.getTime()
      }

      // ── Persistence (Process + base64 for UTF-8 safety) ──────
      function _utf8ToBase64(s) {
          return Qt.btoa(unescape(encodeURIComponent(s)))
      }
      function _base64ToUtf8(b) {
          try { return decodeURIComponent(escape(Qt.atob(b))) }
          catch (e) { return "" }
      }

      Process {
          id: loadProc
          running: true
          command: ["sh", "-c",
              "F=\"$HOME/.local/state/quickshell/appointments.json\"; " +
              "cat \"$F\" 2>/dev/null || true"]
          stdout: StdioCollector { id: loadOut }
          onExited: {
              var txt = (loadOut.text || "").trim()
              if (txt.length === 0) return
              try {
                  var data = JSON.parse(txt)
                  if (data && Array.isArray(data.items)) {
                      appts.clear()
                      for (var i = 0; i < data.items.length; i++)
                          appts.append(root._normalize(data.items[i]))
                      root._bump()
                  }
              } catch (e) {
                  console.warn("appointments: parse failed:", e)
              }
          }
      }

      Process { id: saveProc }

      function save() {
          var arr = []
          for (var i = 0; i < appts.count; i++) {
              var e = appts.get(i)
              arr.push({
                  id: e.id, title: e.title, body: e.body,
                  date: e.date, time: e.time, leadMins: e.leadMins,
                  leadFired: e.leadFired, alarmFired: e.alarmFired
              })
          }
          var json = JSON.stringify({ version: 1, items: arr })
          var encoded = root._utf8ToBase64(json)
          saveProc.command = ["sh", "-c",
              "F=\"$HOME/.local/state/quickshell/appointments.json\"; " +
              "mkdir -p \"$(dirname \"$F\")\"; " +
              "TMP=\"$F.tmp\"; " +
              "printf '%s' \"$1\" | base64 -d > \"$TMP\" && mv \"$TMP\" \"$F\"",
              "sh", encoded]
          saveProc.running = true
      }

      // ── Notification firing (libnotify → D-Bus → our NotificationServer) ──
      Process { id: notifyProc }

      function _fire(item, kind) {
          // kind: "lead" or "alarm"
          var title = (kind === "alarm" ? "󰀠  " : "󰀠  ")
                    + item.title
          var subtitle
          if (kind === "lead") {
              var mins = item.leadMins
              var when = (mins >= 60)
                  ? (Math.floor(mins / 60) + "h" + (mins % 60 > 0 ? " " + (mins % 60) + "m" : ""))
                  : (mins + " min")
              subtitle = "In " + when + " — " + item.time + " on " + item.date
          } else {
              subtitle = "Now — " + item.time + " on " + item.date
          }
          if (item.body && item.body.length > 0)
              subtitle += "\n" + item.body

          var urgency = (kind === "alarm") ? "critical" : "normal"
          notifyProc.command = [
              "notify-send",
              "-a", "Quickshell Appointments",
              "-u", urgency,
              "-i", (kind === "alarm" ? "alarm-symbolic" : "appointment-soon"),
              title, subtitle
          ]
          notifyProc.running = true
      }

      readonly property int maxLateMs: 24 * 60 * 60 * 1000   // don't fire >24h late

      function _check() {
          var nowMs = Date.now()
          for (var i = 0; i < appts.count; i++) {
              var item = appts.get(i)
              var schedMs = root._scheduledMs(item)
              if (schedMs <= 0) continue

              // Lead reminder (only if leadMins > 0)
              if (!item.leadFired && item.leadMins > 0) {
                  var triggerMs = schedMs - item.leadMins * 60000
                  if (nowMs >= triggerMs
                      && nowMs - triggerMs < root.maxLateMs) {
                      root._fire(item, "lead")
                      root.update(item.id, { leadFired: true })
                  }
              }
              // Alarm at scheduled time
              if (!item.alarmFired) {
                  if (nowMs >= schedMs
                      && nowMs - schedMs < root.maxLateMs) {
                      root._fire(item, "alarm")
                      root.update(item.id, { alarmFired: true })
                  }
              }
          }
      }

      Timer {
          id: tickTimer
          interval: 30000
          repeat: true
          running: true
          triggeredOnStart: true
          onTriggered: root._check()
      }

      // ── IPC ──────────────────────────────────────────────────
      IpcHandler {
          target: "appointments"
          function list(): string {
              var arr = []
              for (var i = 0; i < appts.count; i++) {
                  var e = appts.get(i)
                  arr.push({
                      id: e.id, title: e.title, body: e.body,
                      date: e.date, time: e.time, leadMins: e.leadMins,
                      leadFired: e.leadFired, alarmFired: e.alarmFired
                  })
              }
              return JSON.stringify(arr, null, 2)
          }
          function add(date: string, time: string, title: string,
                       body: string, leadMins: int): string {
              return root.add({
                  date: date, time: time, title: title,
                  body: body, leadMins: leadMins
              })
          }
          function remove(id: string) {
              root.remove(id)
          }
          function reload() {
              loadProc.running = true
          }
          function testFire() {
              // Trigger the scheduler manually (for debugging).
              root._check()
          }
      }
  }
''
