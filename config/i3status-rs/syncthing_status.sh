#!/bin/bash
# ~/.config/i3status-rs/syncthing_status.sh

# 1. Check systemd service status
SERVICE_ACTIVE=$(systemctl --user is-active syncthing.service 2>/dev/null)
if [[ "$SERVICE_ACTIVE" != "active" ]]; then
    echo '{"text": "󰓨", "state": "Critical"}'
    exit 0
fi

# 2. Extract API Key
CONFIG_FILE="$HOME/.local/state/syncthing/config.xml"
if [ ! -f "$CONFIG_FILE" ]; then
    echo '{"text": "󰓦", "state": "Critical"}'
    exit 0
fi

API_KEY=$(grep -oP '(?<=<apikey>)[^<]+' "$CONFIG_FILE" | head -n 1)
if [ -z "$API_KEY" ]; then
    echo '{"text": "󰓦", "state": "Critical"}'
    exit 0
fi

# 3. Fetch connections status
CONNECTIONS_JSON=$(curl -s --max-time 1.5 -H "X-API-Key: $API_KEY" http://127.0.0.1:8384/rest/system/connections)
if [ $? -ne 0 ] || [ -z "$CONNECTIONS_JSON" ]; then
    echo '{"text": "󰓦", "state": "Critical"}'
    exit 0
fi

# 4. Check if any remote nodes are online
ANY_CONNECTED=$(echo "$CONNECTIONS_JSON" | jq -r '[.connections[].connected] | any' 2>/dev/null)
if [ "$ANY_CONNECTED" != "true" ]; then
    echo '{"text": "󰓦", "state": "Warning"}'
    exit 0
fi

# 5. Fetch folder list and status
FOLDERS_JSON=$(curl -s --max-time 1.5 -H "X-API-Key: $API_KEY" http://127.0.0.1:8384/rest/config/folders)
if [ $? -ne 0 ] || [ -z "$FOLDERS_JSON" ]; then
    echo '{"text": "󰓦", "state": "Idle"}'
    exit 0
fi

FOLDER_IDS=$(echo "$FOLDERS_JSON" | jq -r '.[].id' 2>/dev/null)
IS_SYNCING=false
for FOLDER_ID in $FOLDER_IDS; do
    STATUS_JSON=$(curl -s --max-time 1 -H "X-API-Key: $API_KEY" "http://127.0.0.1:8384/rest/db/status?folder=$FOLDER_ID")
    STATE=$(echo "$STATUS_JSON" | jq -r '.state' 2>/dev/null)
    if [ "$STATE" = "syncing" ] || [ "$STATE" = "scanning" ]; then
        IS_SYNCING=true
        break
    fi
done

if [ "$IS_SYNCING" = "true" ]; then
    echo '{"text": "󰓦", "state": "Good"}'
else
    echo '{"text": "󰓦", "state": "Idle"}'
fi
