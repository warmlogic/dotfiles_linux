#!/usr/bin/env bash

mkdir $HOME/Downloads
MC_DL_FILE="Miniconda3-latest-Linux-x86_64.sh"
MC_DL_PATH="$HOME/Downloads/$MC_DL_FILE"
MC_DIR="$HOME/miniconda3"

# Download miniconda file only if it does not already exist
{
if [ ! -f "$MC_DL_PATH" ]; then
    wget --show-progress -O $MC_DL_PATH https://repo.continuum.io/miniconda/$MC_DL_FILE
fi
}

# install
bash $MC_DL_PATH -b -p $MC_DIR

# Add to and source .bashrc
export PATH="$MC_DIR/bin:$PATH"

# # Don't ask if you want to update
# conda config --set always_yes yes

# -q for quiet
conda update -q conda

conda install -n root _license

conda upgrade -y --all

# List info in case things don't work
conda info -a

# This adds the conda-forge channel below the defaults library
conda config --append channels conda-forge

# with mkl
packages='pip
mkl
numpy
scipy
scikit-learn
pandas
statsmodels
matplotlib
seaborn
cython
jupyter
notebook
ipywidgets
jupyter_contrib_nbextensions
s3fs
networkx'

conda install $packages -y

# additional packages
pip install -U lightgbm # https://github.com/Microsoft/LightGBM
# pip install -U xgboost
pip install -U imbalanced-learn
pip install -U xlrd
pip install -U tqdm
pip install -U pandas-profiling # https://github.com/pandas-profiling/pandas-profiling
pip install -U pandas-datareader # https://github.com/pydata/pandas-datareader
pip install -U graphviz
pip install -U missingno # https://github.com/ResidentMario/missingno
pip install -U pytest
# pip install -U kaggle-cli

# NLP packages
pip install -U ftfy # https://github.com/LuminosoInsight/python-ftfy
pip install -U nltk
pip install -U spacy # https://spacy.io/
python -m spacy download en_core_web_md
# pip install -U thinc # https://github.com/explosion/thinc
pip install -U gensim # https://radimrehurek.com/gensim/
pip install -U pyldavis # https://github.com/bmabey/pyLDAvis
# pip install -U fuzzywuzzy # https://github.com/seatgeek/fuzzywuzzy
# pip install -U python-Levenshtein # for fuzzywuzzy
# pip install -U textacy # https://github.com/chartbeat-labs/textacy
# pip install -U pattern # https://github.com/clips/pattern

# neural network packages
# pip install -U tensorflow
# pip install -U keras
# http://pytorch.org/
