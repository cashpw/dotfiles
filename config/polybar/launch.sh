#!/usr/bin/env sh
#
# is_work_machine
# is_work_laptop
# is_work_desktop
# is_work_cloudtop
source ~/.scripts/identify_device/identify_device.sh

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use
polybar-msg cmd quit
# Otherwise you can use the nuclear option:
# killall -q polybar

# Launch bar1 and bar2
#echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
#polybar bar1 2>&1 | tee -a /tmp/polybar1.log & disown
#polybar bar2 2>&1 | tee -a /tmp/polybar2.log & disown
echo "---" | tee -a /tmp/polybar1.log
if is_work_desktop; then
  polybar desktop 2>&1 | tee -a /tmp/polybar1.log & disown
fi

echo "Bars launched..."
