#!/usr/bin/env bash
set -e

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "${BASEDIR}"

echo "================================================================"
echo " Running Post-Installation Setup & Environment Profile Tasks"
echo "================================================================"

echo ""
echo "[1/5] Updating Git Submodules..."
git submodule sync --recursive || true
git submodule update --init --recursive || true

echo ""
echo "[2/5] Setting up Environment-Specific Configurations..."
bash scripts/config/setup_env.sh

echo ""
echo "[3/5] Building Zotra Server..."
bash install_zotra.sh

echo ""
echo "[4/5] Setting up Voxtype & Dictation Models..."
if command -v voxtype >/dev/null 2>&1; then
  echo "Voxtype is installed. Updating models..."
  voxtype setup --download --model medium.en --quiet --no-post-install || true
  voxtype setup --download --model base.en --quiet --no-post-install || true
  voxtype setup --quiet vad || true
else
  echo "Voxtype binary not found in PATH yet. Run 'bash scripts/config/install_voxtype.sh' after installing system dependencies."
fi

echo ""
echo "[5/5] Enabling & Starting Systemd User Services..."
if command -v systemctl >/dev/null 2>&1 && [ -n "$XDG_RUNTIME_DIR" ] && [ -d "$XDG_RUNTIME_DIR" ]; then
  systemctl --user daemon-reload || true
  if command -v syncthing >/dev/null 2>&1; then
    echo "Enabling and starting syncthing.service..."
    systemctl --user enable --now syncthing.service || true
  fi
else
  echo "systemctl user daemon not available in current shell session."
fi

echo ""
echo "[✓] Post-installation setup complete!"
