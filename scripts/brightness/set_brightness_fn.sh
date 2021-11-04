#!/bin/bash # Set the brightness of a specific monitor
#
# Expects 2 arguments: a value (0, 1) and a monitor name (north|south|east|west)

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

source "${DIR}/../config/monitors.sh"
source "${DIR}/config.sh"

function update_brightest() {
  brightest_brightness=0

  for direction in "north" "east" "south" "west"; do
    brightness=$(<"${DIR}/${direction}_monitor_brightness.txt")
    if [[ $brightness > $brightest_brightness ]]; then
      brightest_brightness=$brightness
    fi
  done

  echo $brightest_brightness > "${DIR}/brightest_brightness.txt"
}

function set_brightness() {
  if [ "$#" -ne 2 ]; then
    echo "You must enter exactly 2 command line arguments" 1>&2
    exit
  fi

  new_brightness=$1
  direction=$2

  monitor_crtc_number=""
  if [[ $direction == "north" ]]; then
    monitor_crtc_number=$north_monitor_crtc_number
  elif [[ $direction == "south" ]]; then
    monitor_crtc_number=$south_monitor_crtc_number
  elif [[ $direction == "east" ]]; then
    monitor_crtc_number=$east_monitor_crtc_number
  elif [[ $direction == "west" ]]; then
    monitor_crtc_number=$west_monitor_crtc_number
  fi

  # Update stored brightness for monitor
  echo $new_brightness > "${DIR}/${direction}_monitor_brightness.txt"

  # Update the stored brightest brightness, if necessary
  update_brightest

  redshift -P -O $temperature -b $new_brightness -m randr:crtc=$monitor_crtc_number
}
