#!/bin/bash
# Increase the brightness (optionally: of a specific monitor
#
# Expects 1-2 arguments: a direction (up|down) and an optional monitor name

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

BRIGHTNESS_STEP=0.1
TEMPERATURE=3000

source "${DIR}/../config/monitors.sh"
source "${DIR}/config.sh"
source "${DIR}/set_brightness_fn.sh"

function set_brightness() {
  local monitor_name="${1}"
  local monitor_crtc_number="$(determine_crtc_number $monitor_name)"
  local brightness="$(clamp_brightness $2)"
  local temperature="${3:-$TEMPERATURE}"

  redshift -P -O $temperature -b $brightness -m randr:crtc=$monitor_crtc_number

  update_stored_brightness "${monitor_name}" "${brightness}"
}

function update_stored_brightness() {
  local monitor_name="$1"
  local brightness="$2"

  echo "${brightness}" > "${DIR}/${monitor_name}_monitor_brightness.txt"
}

function get_stored_brightness() {
  local monitor_name="$1"

  cat "${DIR}/${monitor_name}_monitor_brightness.txt"
}

function clamp_brightness() {
  echo $(python3 -c "from decimal import Decimal; print(min(max(Decimal(${1}), 0.1), 1.0))")
}

function step_up_brightness() {
  local monitor_name="${1}"
  local current_brightness="$(get_stored_brightness $monitor_name)"
  local brightness="$(python3 -c "print($current_brightness+$BRIGHTNESS_STEP)")"
  local monitor_crtc_number="$(determine_crtc_number $monitor_name)"
  local temperature="${2:-$TEMPERATURE}"

  set_brightness "${monitor_name}" "${brightness}" "${temperature}"
}

function step_down_brightness() {
  local monitor_name="${1}"
  local current_brightness="$(get_stored_brightness $monitor_name)"
  local brightness="$(python3 -c "print($current_brightness-$BRIGHTNESS_STEP)")"
  local monitor_crtc_number="$(determine_crtc_number $monitor_name)"
  local temperature="${2:-$TEMPERATURE}"

  set_brightness "${monitor_name}" "${brightness}" "${temperature}"
}

function determine_crtc_number() {
  if [[ "$1" == "left" ]]; then
    echo $left_monitor_crtc_number
  elif [[ "$1" == "center" ]]; then
    echo $center_monitor_crtc_number
  elif [[ "$1" == "right" ]]; then
    echo $right_monitor_crtc_number
  else
    echo ""
  fi
}

function create_brightness_files_if_not_exist() {
  local files=(
    "${DIR}/left_monitor_brightness.txt"
    "${DIR}/center_monitor_brightness.txt"
    "${DIR}/right_monitor_brightness.txt"
  )

  for f in "${files[@]}"; do
    if [[ ! -f "${file}" ]]; then
      echo "1.0" > "${file}"
    fi
  done
}
 
if [[ "$#" < 1 || "$#" > 3 ]]; then
  echo "Wrong number of arguments."
  echo "Expected: up|down (left|center|right)"
  exit 1
fi

brightness_direction="${1}"
monitor_name="${2}"

monitor_crtc_number="$(determine_crtc_number $monitor_name)"
if [[ -z $monitor_crtc_number ]]; then
  # Change the brightness of all monitors

  if [[ "${brightness_direction}" == "up" ]]; then
    step_up_brightness "left"
    step_up_brightness "center"
    step_up_brightness "right"
  fi

  if [[ "${brightness_direction}" == "down" ]]; then
    step_down_brightness "left"
    step_down_brightness "center"
    step_down_brightness "right"
  fi

  exit 0
fi

if [[ "${brightness_direction}" == "up" ]]; then
  step_up_brightness "${monitor_name}"
fi

if [[ "${brightness_direction}" == "down" ]]; then
  step_down_brightness "${monitor_name}"
fi
