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

# Source NVM if present
export NVM_DIR="$HOME/.config/nvm"
if [ -s "$NVM_DIR/nvm.sh" ]; then
  # Sourcing nvm.sh may set non-zero return under set -e if nvm environment has warnings, so allow || true
  \. "$NVM_DIR/nvm.sh" || true
fi

# Auto-install Node/NPM via NVM if nvm is available but no node version is active yet
if ! command -v npm >/dev/null 2>&1 && command -v nvm >/dev/null 2>&1; then
  echo "Installing Node.js LTS via NVM..."
  nvm install --lts
fi

if command -v npm >/dev/null 2>&1; then
  echo "Installing Zotra npm dependencies..."
  npm install --production --quiet
  echo "Zotra Server setup complete."
else
  echo "WARNING: 'npm' is not installed and NVM was not found in $NVM_DIR."
  echo "Install NVM via ./install or run: curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash"
fi
