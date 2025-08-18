#!/bin/bash
# Take a screenshot of the selected area

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Ensure save directory exists
save_dir="/home/cashpw/Pictures/screenshots"
mkdir -p $save_dir

if [[ "$XDG_CURRENT_DESKTOP" == "sway" ]]; then
  grim -g "$(slurp)" "${save_dir}/screenshot-$(date +%Y-%m-%d-%H-%M-%S).png"
else
  maim -m 10 -s "${save_dir}/screenshot-$(date +%Y-%m-%d-%H-%M-%S).png"
fi

notify-send -t 3000 "Screenshot saved."
