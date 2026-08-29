#!/usr/bin/env bash
set -e

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "${BASEDIR}"

echo "================================================================"
echo " Running Post-Installation Setup & Environment Profile Tasks"
echo "================================================================"

echo ""
echo "[1/4] Updating Git Submodules..."
git submodule update --init --recursive

echo ""
echo "[2/4] Setting up Environment-Specific Configurations..."
bash scripts/config/setup_env.sh

echo ""
echo "[3/4] Building Zotra Server..."
bash install_zotra.sh

echo ""
echo "[4/4] Setting up Voxtype & Dictation Models..."
if command -v voxtype >/dev/null 2>&1; then
  echo "Voxtype is installed. Updating models..."
  voxtype setup --download --model medium.en --quiet --no-post-install || true
  voxtype setup --download --model base.en --quiet --no-post-install || true
  voxtype setup --quiet vad || true
else
  echo "Voxtype binary not found in PATH yet. Run 'bash scripts/config/install_voxtype.sh' after installing system dependencies."
fi

echo ""
echo "[✓] Post-installation setup complete!"
