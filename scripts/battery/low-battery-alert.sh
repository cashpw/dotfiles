#!/bin/sh

# Find all battery devices
for battery in /sys/class/power_supply/BAT*; do
    # Get capacity and status
    capacity=$(cat "$battery/capacity")
    status=$(cat "$battery/status")

    # Check if battery is discharging and capacity is 10% or less
    if [ "$status" = "Discharging" ] && [ "$capacity" -le 10 ]; then
        # Send a critical notification
        notify-send -u critical --icon=battery_1_bar_64dp_E3E3E3_FILL0_wght400_GRAD0_opsz48 "Low Battery" "Battery level is at ${capacity}%"
    fi
done
