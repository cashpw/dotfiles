#!/usr/bin/env sh

source ~/.scripts/identify_device/identify_device.sh

if is_work_laptop; then
  source ~/.scripts/utility/lock.sh && systemctl suspend
else
  source ~/.scripts/utility/lock.sh
fi
