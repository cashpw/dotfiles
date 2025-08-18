#!/usr/bin/env sh

bg_color=1d1f21
if [[ "$XDG_CURRENT_DESKTOP" == "sway" ]]; then
  swaylock --ignore-empty-password --color=$bg_color
elif [[ "$XDG_CURRENT_DESKTOP" == "sway" ]]; then
  i3lock --ignore-empty-password --color=$bg_color
fi
