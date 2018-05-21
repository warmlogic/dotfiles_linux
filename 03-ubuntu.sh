#!/usr/bin/env bash

# This script is designed to work with ubuntu 16.04 LTS
# based on https://github.com/fastai/courses/blob/master/setup/install-gpu.sh

# ensure system is updated and has basic build tools
sudo apt-get update
sudo apt-get --assume-yes upgrade
sudo apt-get --assume-yes install build-essential gcc g++ make binutils htop tmux emacs software-properties-common swig
