#!/usr/bin/env bash

start_timer() {
  local duration="$1"
  while [ "$duration" -gt 0 ]; do
    # Send the current duration to EWW to update the timer label
    eww update timer-countdown="$duration seconds left"
    sleep 1
    ((duration--))
  done
  # Reset the timer countdown in EWW to default
  eww update timer-countdown="Enter timer duration"
  # Notify the user when the timer is done
  notify-send "Timer finished!"
}

DURATION_IN_SECONDS="$1"
start_timer "$DURATION_IN_SECONDS"
