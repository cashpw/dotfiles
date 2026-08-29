#!/usr/bin/env bash
set -e

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ZOTRA_REPO_DIR="${BASEDIR}/local/share/zotra-server"
ZOTRA_LINK_DIR="$HOME/.local/share/zotra-server"

echo "Setting up Zotra Server..."

if [ -d "${ZOTRA_REPO_DIR}" ]; then
  echo "Updating Zotra submodules recursively from repo..."
  git -C "${BASEDIR}" config --unset-all submodule.zotra-server.url 2>/dev/null || true
  git -C "${BASEDIR}" config --remove-section submodule.zotra-server 2>/dev/null || true
  git -C "${BASEDIR}" submodule update --init --recursive local/share/zotra-server
fi

if [ ! -d "${ZOTRA_LINK_DIR}" ]; then
  echo "Error: Directory ${ZOTRA_LINK_DIR} does not exist."
  echo "Ensure dotbot symlinks have been installed."
  exit 1
fi

cd "${ZOTRA_LINK_DIR}"

if command -v npm >/dev/null 2>&1; then
  echo "Installing Zotra npm dependencies..."
  npm install --production --quiet
  echo "Zotra Server setup complete."
else
  echo "WARNING: 'npm' is not installed. Run 'sudo dnf install npm nodejs' or 'sudo apt install npm' then rerun ./install."
fi
