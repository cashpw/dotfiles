#!/bin/sh
xrandr --output DVI-D-1 --off --output DP-1 --mode 2560x1440 --pos 2880x0 --rotate right --output DP-2 --mode 2560x1440 --pos 1440x0 --rotate left --output HDMI-1 --off --output DP-3 --mode 2560x1440 --pos 0x0 --rotate left

xrandr --setmonitor TwoPortrait auto DP-2,DP-1
