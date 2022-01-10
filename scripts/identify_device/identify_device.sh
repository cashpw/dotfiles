# Identify the current device
#
# Note: These files are manually created.

function is_local_work() {
  [[ -f "$HOME/.config/cashweaver-device-name-local-work" ]]
}

function is_remote_work() {
  [[ -f "$HOME/.config/cashweaver-device-name-remote-work" ]]
}

function is_laptop_work() {
  [[ -f "$HOME/.config/cashweaver-device-name-laptop-work" ]]
}
