#!/bin/bash

# Target the default audio sink (output) in PipeWire
SINK="@DEFAULT_AUDIO_SINK@"

# Limit max volume to 100% to prevent audio clipping/hardware damage.
# You can change this to 1.5 if you want to allow up to 150% volume boost.
MAX_VOL="1.0"

show_help() {
    echo "Usage: $0 [COMMAND] [VALUE]"
    echo "Commands:"
    echo "  set <0-100>    Set volume to a specific percentage (e.g., $0 set 50)"
    echo "  up <number>    Increase volume by percentage (e.g., $0 up 5)"
    echo "  down <number>  Decrease volume by percentage (e.g., $0 down 5)"
    echo "  mute           Mute audio completely"
    echo "  unmute         Unmute audio"
    echo "  toggle         Toggle mute state on/off"
}

if [ -z "$1" ]; then
    show_help
    exit 1
fi

case "$1" in
    set)
        if [[ -n "$2" && "$2" =~ ^[0-9]+$ ]]; then
            wpctl set-volume "$SINK" "$2%"
            echo "Volume set to $2%"
        else
            echo "Error: 'set' requires a numeric value."
        fi
        ;;
    up)
        if [[ -n "$2" && "$2" =~ ^[0-9]+$ ]]; then
            # -l ensures we do not exceed the MAX_VOL limit
            wpctl set-volume -l "$MAX_VOL" "$SINK" "$2%+"
            echo "Volume increased by $2%"
        else
            echo "Error: 'up' requires a numeric value."
        fi
        ;;
    down)
        if [[ -n "$2" && "$2" =~ ^[0-9]+$ ]]; then
            wpctl set-volume "$SINK" "$2%-"
            echo "Volume decreased by $2%"
        else
            echo "Error: 'down' requires a numeric value."
        fi
        ;;
    mute)
        wpctl set-mute "$SINK" 1
        echo "Audio Muted"
        ;;
    unmute)
        wpctl set-mute "$SINK" 0
        echo "Audio Unmuted"
        ;;
    toggle)
        wpctl set-mute "$SINK" toggle
        echo "Mute Toggled"
        ;;
    *)
        show_help
        ;;
esac
