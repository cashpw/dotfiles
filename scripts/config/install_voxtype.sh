#!/usr/bin/env bash
set -e

# Compatibility checks
ARCH=$(uname -m)
if [ "$ARCH" != "x86_64" ]; then
    echo "Error: Voxtype installation script currently only supports x86_64 automatically. (Detected: $ARCH)"
    exit 1
fi

if ! grep -q avx2 /proc/cpuinfo; then
    echo "WARNING: CPU does not support AVX2. Voxtype binary may crash."
fi

# Input group check (required for Vox internal hotkeys)
if ! groups | grep -q "\binput\b"; then
    echo "WARNING: You are not in the 'input' group. Voxtype's internal hotkeys will not work."
    echo "Please run: sudo usermod -aG input \$USER"
    echo "And then log out and log back in."
fi

TARGET_DIR="$HOME/.local/bin"
TARGET_BIN="$TARGET_DIR/voxtype"

mkdir -p "$TARGET_DIR"

if [ ! -f "$TARGET_BIN" ]; then
    echo "Searching for latest Voxtype Linux AVX2 binary..."
    DOWNLOAD_URL=$(curl -s https://api.github.com/repos/peteonrails/voxtype/releases | \
      grep -o 'https://github.com/peteonrails/voxtype/releases/download/[^"]*linux-x86_64-avx2' | \
      head -n 1)

    if [ -z "$DOWNLOAD_URL" ]; then
        echo "Failed to find a suitable release dynamically. Falling back to v0.7.5."
        VERSION="0.7.5"
        DOWNLOAD_URL="https://github.com/peteonrails/voxtype/releases/download/v${VERSION}/voxtype-${VERSION}-linux-x86_64-avx2"
    fi

    echo "Downloading Voxtype from: $DOWNLOAD_URL"
    curl -L -o "$TARGET_BIN" "$DOWNLOAD_URL"
    chmod +x "$TARGET_BIN"
    echo "Voxtype installed to $TARGET_BIN"
else
    echo "Voxtype is already installed."
fi

# Download models
# Note: These commands might take some time and output progress.
echo "Checking/Downloading VoxType models..."
"$TARGET_BIN" setup --download --model medium.en --quiet --no-post-install
"$TARGET_BIN" setup --download --model base.en --quiet --no-post-install
"$TARGET_BIN" setup --quiet vad
echo "VoxType models setup complete."
