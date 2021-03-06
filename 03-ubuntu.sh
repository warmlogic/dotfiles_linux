#!/usr/bin/env bash

# This script is designed to work with ubuntu 16.04 LTS
# based on https://github.com/fastai/courses/blob/master/setup/install-gpu.sh

# ensure system is updated and has basic build tools
sudo apt-get update
sudo apt-get --assume-yes upgrade
sudo apt-get --assume-yes install build-essential gcc g++ make binutils htop tmux emacs software-properties-common swig unzip
sudo apt-get --assume-yes install python-pydot python-pydot-ng graphviz

# Fonts for matplotlib
sudo apt-get --assume-yes install font-manager

# Optional: MS fonts
echo ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true | sudo debconf-set-selections
sudo apt-get --assume-yes install ttf-mscorefonts-installer
# remove with: sudo apt-get remove --purge ttf-mscorefonts-installer

# # Visual Studio Code - Linux app
# curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
# sudo install -o root -g root -m 644 microsoft.gpg /etc/apt/trusted.gpg.d/
# sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
# sudo apt-get install apt-transport-https
# sudo apt-get update
# sudo apt-get --assume-yes install code

# For editing in Visual Studio Code on a remote computer
# Requires additional setup: http://blog.macromates.com/2011/mate-and-rmate/
# 1. Add to ~/.ssh/config entry: RemoteForward 52698 localhost:52698
# 2. SSH into server
# 3. Run to edit in Visual Studio Code: rcode myfile.txt
sudo wget -O /usr/local/bin/rcode https://raw.github.com/aurora/rmate/master/rmate
sudo chmod +x /usr/local/bin/rcode

# For editing in SublimeText on a remote computer
# Requires additional setup: http://blog.macromates.com/2011/mate-and-rmate/
# 1. Add to ~/.ssh/config entry: RemoteForward 52698 localhost:52698
# 2. SSH into server
# 3. Run to edit in SublimeText: rsub myfile.txt
sudo wget -O /usr/local/bin/rsub https://raw.github.com/aurora/rmate/master/rmate
sudo chmod +x /usr/local/bin/rsub

# # spell checking with pyenchant
# sudo apt-get --assume-yes install enchant

# grammar checking with LanguageTool (via language_check in Python)
# sudo apt-get install default-jre
