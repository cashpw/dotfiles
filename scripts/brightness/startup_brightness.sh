#!/bin/bash
# Increase the brightness (optionally: of a specific monitor
#
# Expects 1-2 arguments: a direction (up|down) and an optional monitor name

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

source "${DIR}/../config/monitors.sh"
source "${DIR}/config.sh"

monitor_crtc_number=""
if [[ ! -z $2 ]]; then
  if [[ $2 == "north" ]]; then
    monitor_crtc_number=$north_monitor_crtc_number
  elif [[ $2 == "south" ]]; then
    monitor_crtc_number=$south_monitor_crtc_number
  elif [[ $2 == "west" ]]; then
    monitor_crtc_number=$west_monitor_crtc_number
  fi
fi

if [[ -z $monitor_crtc_number ]]; then
  # Change the brightness of all monitors

  brightest_brightness=$(<"${DIR}/brightest_brightness.txt")
  if [[ $1 == "up" ]]; then
    new_brightness=$(python3 -c "from decimal import Decimal; print(max(min(Decimal(${brightest_brightness}+${brightness_step}), 1), 0.1))")
  else
    new_brightness=$(python3 -c "from decimal import Decimal; print(max(min(Decimal(${brightest_brightness}-${brightness_step}), 1), 0.1))")
  fi

  # Update all stored brightnesses
  echo $new_brightness > "${DIR}/brightest_brightness.txt"
  echo $new_brightness > "${DIR}/north_monitor_brightness.txt"
  echo $new_brightness > "${DIR}/south_monitor_brightness.txt"
  echo $new_brightness > "${DIR}/west_monitor_brightness.txt"

  redshift -P -O $temperature -b $new_brightness
else

  # Change the brightness of the specified monitor
  current_brightness=$(<"${DIR}/${2}_monitor_brightness.txt")
  if [[ $1 == "up" ]]; then
    new_brightness=$(python3 -c "from decimal import Decimal; print(max(min(Decimal(${current_brightness}+${brightness_step}), 1), 0.1))")
  else
    new_brightness=$(python3 -c "from decimal import Decimal; print(max(min(Decimal(${current_brightness}-${brightness_step}), 1), 0.1))")
  fi

  # Update brightness for this monitor
  echo $new_brightness > "${DIR}/${2}_monitor_brightness.txt"

  # Update the brightest brightness if necessary
  brightest_brightness=$(<"${DIR}/brightest_brightness.txt")
  if [[ $new_brightness > $brightest_brightness ]]; then
    echo $new_brightness > "${DIR}/brightest_brightness.txt"
  fi

  redshift -P -O $temperature -b $new_brightness -m randr:crtc=$monitor_crtc_number
fi
