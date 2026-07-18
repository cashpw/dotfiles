#!/bin/bash
XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
STATE_FILE="$XDG_RUNTIME_DIR/voxtype/state"

# Check systemd user service status
SERVICE_STATE=$(systemctl --user show voxtype.service --property=ActiveState 2>/dev/null)

if [[ "$SERVICE_STATE" != "ActiveState=active" ]]; then
    # Check if failed or just stopped
    IS_FAILED=$(systemctl --user show voxtype.service --property=SubState 2>/dev/null)
    if [[ "$IS_FAILED" == "SubState=failed" ]]; then
        echo '{"text": " ⚠️ VOX FAILED ", "state": "Critical"}'
    else
        echo '{"text": " ⚠️ VOX DEAD ", "state": "Warning"}'
    fi
    exit 0
fi

if [ ! -f "$STATE_FILE" ]; then
    echo '{"text": "🎙️", "state": "Idle"}'

    exit 0
fi

STATE=$(cat "$STATE_FILE")

case "$STATE" in
    "recording")
        echo '{"text": " 🔴 RECORDING ", "state": "Critical"}'
        ;;
    "transcribing")
        echo '{"text": " ⏳ TRANSCRIBING ", "state": "Warning"}'
        ;;
    "idle")
        echo '{"text": "🎙️", "state": "Idle"}'

        ;;
    *)
        echo '{"text": "🎙️", "state": "Idle"}'

        ;;
esac
