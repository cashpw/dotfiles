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

# Clean up old autostart symlink if it exists
rm -f "$HOME/.config/sway/config.d/autostart-env.conf"

if is_work_machine; then
  echo "Setting up configuration for WORK environment..."
  
  # Sway Environment Config
  ln -sf "$HOME/.config/sway/config.d/env-work.conf" "$HOME/.config/sway/config.d/env.conf"
  
  # Kanshi Profile
  ln -sf "$HOME/.config/kanshi/config-work" "$HOME/.config/kanshi/config"

  # Voxtype Config
  ln -sf "$HOME/.config/voxtype/config-work.toml" "$HOME/.config/voxtype/config.toml"

else
  echo "Setting up configuration for PERSONAL environment..."
  
  # Sway Environment Config
  ln -sf "$HOME/.config/sway/config.d/env-personal.conf" "$HOME/.config/sway/config.d/env.conf"
  
  # Kanshi Profile
  ln -sf "$HOME/.config/kanshi/config-personal" "$HOME/.config/kanshi/config"

  # Voxtype Config
  ln -sf "$HOME/.config/voxtype/config-personal.toml" "$HOME/.config/voxtype/config.toml"
fi
