#!/usr/bin/env sh
#
# Maybe backup a directory to Google Drive.
#
# Create a backup tar file. Compare it's checksum against the checksum of the
# most recent run and only upload if the checksums differ.

BASE="$(basename $(pwd))"
DRIVE_DIR="/mnt/cashbweaver-gdrive/backups"
BACKUP_FILE_NAME="${BASE}.tar"
CHECKSUM_FILE_NAME="checksum-${BASE}.txt"

tar --create --file="/tmp/$BACKUP_FILE_NAME" .
trap "rm /tmp/$BACKUP_FILE_NAME" EXIT
cd /tmp

if [ -f "$CHECKSUM_FILE_NAME" ]; then
    CHECKSUM="$(sha1sum "$BACKUP_FILE_NAME" | awk '{ print $1 }')"
    PREVIOUS_CHECKSUM="$(<$CHECKSUM_FILE_NAME)"

    if [[ "$CHECKSUM" == "$PREVIOUS_CHECKSUM" ]]; then
        # Skip upload if checksums match
        exit 0
    fi
fi

sha1sum "$BACKUP_FILE_NAME" | awk '{ print $1 }' > "$CHECKSUM_FILE_NAME"
rm "${DRIVE_DIR}/${BACKUP_FILE_NAME}"
cp "$BACKUP_FILE_NAME" "$DRIVE_DIR"
