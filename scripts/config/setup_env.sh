#!/usr/bin/env bash

# Source the device identification script
source "$HOME/.scripts/identify_device/identify_device.sh"

# Ensure directories exist
mkdir -p "$HOME/.config/sway/config.d"
mkdir -p "$HOME/.config/kanshi"

if is_work_machine; then
  echo "Setting up for WORK environment..."
  
  # Sway Autostart
  ln -sf "$HOME/.config/sway/config.d/autostart-work.conf" "$HOME/.config/sway/config.d/autostart-env.conf"
  
  # Kanshi Profile
  ln -sf "$HOME/.config/kanshi/config-work" "$HOME/.config/kanshi/config"

else
  echo "Setting up for PERSONAL environment..."
  
  # Sway Autostart
  ln -sf "$HOME/.config/sway/config.d/autostart-personal.conf" "$HOME/.config/sway/config.d/autostart-env.conf"
  
  # Kanshi Profile
  ln -sf "$HOME/.config/kanshi/config-personal" "$HOME/.config/kanshi/config"
fi
