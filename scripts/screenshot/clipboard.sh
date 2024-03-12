#!/bin/bash
# Take a screenshot of the selected area

maim -m 10 -s | xclip -selection clipboard -t image/png

notify-send -t 3000 "Screenshot copied to clipboard."
