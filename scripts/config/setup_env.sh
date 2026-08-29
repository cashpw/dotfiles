#!/usr/bin/env bash

# Source the device identification script
if [ -f "$HOME/.scripts/identify_device/identify_device.sh" ]; then
  source "$HOME/.scripts/identify_device/identify_device.sh"
else
  # Fallback if scripts are not linked yet
  source "$(dirname "${BASH_SOURCE[0]}")/../identify_device/identify_device.sh"
fi

# Ensure target configuration directories exist
mkdir -p "$HOME/.config/sway/config.d"
mkdir -p "$HOME/.config/kanshi"
mkdir -p "$HOME/.config/voxtype"
mkdir -p "$HOME/.config/i3status-rs"

# Clean up old autostart symlink if it exists
rm -f "$HOME/.config/sway/config.d/autostart-env.conf"

if is_work_machine; then
  echo "Setting up configuration for WORK environment..."
  
  # Sway Environment Config
  (cd "$HOME/.config/sway/config.d" && ln -sf env-work.conf env.conf)
  
  # Kanshi Profile
  (cd "$HOME/.config/kanshi" && ln -sf config-work config)

  # Voxtype Config
  (cd "$HOME/.config/voxtype" && ln -sf config-work.toml config.toml)

  # i3status-rs Config (includes cloudtop and notify-tunnel)
  (cd "$HOME/.config/i3status-rs" && ln -sf config-work.toml config.toml)

else
  echo "Setting up configuration for PERSONAL environment..."
  
  # Sway Environment Config
  (cd "$HOME/.config/sway/config.d" && ln -sf env-personal.conf env.conf)
  
  # Kanshi Profile
  (cd "$HOME/.config/kanshi" && ln -sf config-personal config)

  # Voxtype Config
  (cd "$HOME/.config/voxtype" && ln -sf config-personal.toml config.toml)

  # i3status-rs Config (omits cloudtop and notify-tunnel)
  (cd "$HOME/.config/i3status-rs" && ln -sf config-personal.toml config.toml)
fi
