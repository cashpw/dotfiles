#!/usr/bin/env sh

ANKI_BACKUP_DIR_PATH="$HOME/.local/share/Anki2/User 1/backups"
cd "${ANKI_BACKUP_DIR_PATH}"

LATEST_BACKUP_NAME="$(ls | sort --reverse | head -n1)"

read -r -p "Back up ${LATEST_BACKUP_NAME}? [y/N] " response
if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
    echo "Backing ${LATEST_BACKUP_NAME} up!"
    systemctl --user start cashbweaver-gdrive.service
    cp "${ANKI_BACKUP_DIR_PATH}/${LATEST_BACKUP_NAME}" /mnt/cashbweaver-gdrive/Anki/Backups/
fi
