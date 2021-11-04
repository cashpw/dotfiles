#!/usr/bin/env bash

# Download and install fonts

FONTS_PATH="$HOME/.fonts"
DOWNLOADS_PATH="$HOME/Downloads"

# ref: https://gist.github.com/lukechilds/a83e1d7127b78fef38c2914c4ececc3c
latest_version() {
  curl -sI https://github.com/$1/releases/latest |
    grep -Po 'tag\/\K(v\S+)' |
    sed 's/^v//'
}

if [[ ! -d "$FONTS_PATH" ]]; then
  mkdir ~/.fonts
fi

if [[ ! -d "$DOWNLOADS_PATH" ]]; then
  mkdir ~/Downloads
fi

echo -e "\nAll fonts are downloaded to $DOWNLOADS_PATH."

if [[ ! -d "${FONTS_PATH}/iosevka" ]]; then
  echo "Downloading Iosevka..."
  version="$(latest_version "be5invis/iosevka")"
  iosevka_target="ttf-iosevka-term-ss09-${version}"
  cd ~/Downloads
  wget "https://github.com/be5invis/Iosevka/releases/download/v${version}/${iosevka_target}.zip"
  echo "Installing..."
  mkdir -p ~/.fonts/iosevka
  unzip "${iosevka_target}.zip" -d ${iosevka_target} > /dev/null
  mv "${iosevka_target}/ttf/"* "${FONTS_PATH}/iosevka"
else
  echo "Iosevka has already been installed."
fi
