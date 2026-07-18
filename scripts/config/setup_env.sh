#!/usr/bin/env bash

# Source the device identification script
source "$HOME/.scripts/identify_device/identify_device.sh"

# Ensure directories exist
mkdir -p "$HOME/.config/sway/config.d"
mkdir -p "$HOME/.config/kanshi"

# Clean up old autostart symlink if it exists
rm -f "$HOME/.config/sway/config.d/autostart-env.conf"

if is_work_machine; then
  echo "Setting up for WORK environment..."
  
  # Sway Environment Config
  ln -sf env-work.conf "$HOME/.config/sway/config.d/env.conf"
  
  # Kanshi Profile
  ln -sf config-work "$HOME/.config/kanshi/config"

  # Voxtype Config
  ln -sf config-work.toml "$HOME/.config/voxtype/config.toml"

else
  echo "Setting up for PERSONAL environment..."
  
  # Sway Environment Config
  ln -sf env-personal.conf "$HOME/.config/sway/config.d/env.conf"
  
  # Kanshi Profile
  ln -sf config-personal "$HOME/.config/kanshi/config"

  # Voxtype Config
  ln -sf config-personal.toml "$HOME/.config/voxtype/config.toml"
fi
