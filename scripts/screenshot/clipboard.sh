#!/bin/bash
# Take a screenshot of the selected area

if [[ "$XDG_CURRENT_DESKTOP" == "sway" ]]; then
  grim -g "$(slurp)" - | wl-copy
else
  maim -m 10 -s | xclip -selection clipboard -t image/png
fi

notify-send -t 3000 "Screenshot copied to clipboard."
