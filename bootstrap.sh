# Use this document to setup a new Mac.
# This is not meant to be run as a shell-script,
# rather, as a guide.

# Sign in to iCloud
# Sign in to iTunes Match

# Check for software updates
# Install ad blocking
# Change caps-lock to control

# Copy documents from backup

# Installs the developer command line tools
xcode-select --install

# set default shell
chsh -s /bin/zsh

# Set up ssh keys

# install homebrew - worth checking hompage
# for, potentially, updated instructions
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew install emacs --with-cocoa
brew install fzf
brew install git
brew install ispell
brew install pandoc
brew install par
brew install tree
brew install vim
brew install zsh

# Install ~/code/other-repos from backup

# Install princeXML
# make install textplay (symlink)

# pull-down your dotfiles, or copy from backup

mkdir ~/.config

# Symlink a bunch of things - run from $HOME
ln -s ~/code/dotfiles/git/gitconfig .gitconfig;
ln -s ~/code/dotfiles/git/gitignore_global .gitignore_global;
ln -s ~/code/dotfiles/zsh/zshrc .zshrc;
ln -s ~/code/dotfiles/vim .vim;
ln -s ~/code/dotfiles/emacs ~/.emacs.d;
ln -s ~/code/dotfiles/hammerspoon ~/.hammerspoon;

# Install vim plugin manager + plugins
# Reconstruct ~/.marks directory

# Install apps:

# Adobe Lightroom
# Arq
# BBEdit
# Dropbox
# Emacs
# Gitup
# Google Chrome
# Hammerspoon
# Karabiner
# Name Mangler (Mac App Store)
# Pixelmator (Mac App Store)
# QuickTime Player 7
# Slack
# SuperDuper
# Transmit

# Install work windows-path tools
# Install fonts from server

# ----------------------------------------------------------

# They keyboard setup is rather spesific.
# At the moment Karabiner doesn't work with MacOS Sierra,
# so I'm using Karabiner Elements to remap 2 things:
# 1. Caps lock -> Left control
# 2. Right option -> right control
# I would much prefer my old setup, which was:
# a. Caps -> Escape/Control
# b. Return -> Return/Control
# c. Right option -> hyper
#
# At the moment I'm faking Caps -> Escape/Control via Karabiner + Hammerspoon
# but there's some lag, which is dissapointing, and Return -> Return/Control is impossible.
