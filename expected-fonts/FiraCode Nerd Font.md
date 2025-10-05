# Linux

cd ~/Downloads
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/FiraCode.zip
mkdir -p ~/.local/share/fonts/FiraCodeNerdFont
unzip FiraCode.zip -d ~/.local/share/fonts/FiraCodeNerdFont
sudo fc-cache -v
