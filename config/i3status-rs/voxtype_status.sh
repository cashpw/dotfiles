#!/bin/bash
XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
STATE_FILE="$XDG_RUNTIME_DIR/voxtype/state"

# Check systemd user service status
if ! command -v systemctl &>/dev/null; then
    echo '{"text": "󰍬️ (no systemd)", "state": "Idle"}'
    exit 0
fi

while IFS='=' read -r key val; do
    case "$key" in
    ActiveState) SERVICE_ACTIVE="$val" ;;
    SubState) SERVICE_SUBSTATE="$val" ;;
    esac
done < <(systemctl --user show voxtype.service --property=ActiveState,SubState 2>/dev/null)

if [[ "$SERVICE_ACTIVE" != "active" ]]; then
    if [[ "$SERVICE_SUBSTATE" == "failed" ]]; then
        echo '{"text": "󰍭", "state": "Critical"}'
    else
        echo '{"text": "󰍭", "state": "Warning"}'
    fi
    exit 0
fi

if [ ! -f "$STATE_FILE" ]; then
    echo '{"text": "󰍬️", "state": "Idle"}'
    exit 0
fi

STATE=$(cat "$STATE_FILE")

case "$STATE" in
"recording")
    echo '{"text": "🔴", "state": "Critical"}'
    ;;
"transcribing")
    echo '{"text": "", "state": "Warning"}'
    ;;
"idle")
    echo '{"text": "󰍬", "state": "Idle"}'
    ;;
*)
    echo '{"text": "󰍬", "state": "Idle"}'
    ;;
esac
