# Use this document to setup a new Mac.
# This is not meant to be run as a shell-script,
# rather, as a guide.

# Sign in to iCloud
# Sign in to iTunes Match

# Check for software updates
# Install ad blocking
# Install Little Snitch
# Create separate local password (iCloud password used by default)
# Change caps-lock to control
# Turn on trackpad tap to click
# Disable guest account

# Copy documents from backup

# Installs the developer command line tools
xcode-select --install

# set default shell
chsh -s /bin/zsh

# install homebrew - worth checking hompage
# for, potentially, updated instructions
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew install vim
brew install par
brew install git
brew install tree
brew install ranger
brew install tmux
brew install reattach-to-user-namespace
brew install pandoc
brew install zsh
brew install zsh-syntax-highlighting

# Install princeXML
# make install textplay (symlink)
# Set up ssh keys

# Set Help Viewer windows to non-floating mode
defaults write com.apple.helpviewer DevMode -bool true

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# pull-down your dotfiles, or copy from backup

# Symlink a bunch of things - run from $HOME
ln -s ~/code/dotfiles/git/gitconfig .gitconfig
ln -s ~/code/dotfiles/git/gitignore_global .gitignore_global
ln -s ~/code/dotfiles/zsh .zsh
ln -s ~/code/dotfiles/zsh/zshrc .zshrc
ln -s ~/code/dotfiles/vim .vim
ln -s ~/code/dotfiles/vim/vimrc .vimrc
ln -s ~/code/dotfiles/Karabiner ~/Library/Application\ Support/Karabiner
ln -s ~/code/dotfiles/tmux.conf .tmux.conf
ln -s ~/code/dotfiles/slate .slate

# Install vim plugin manager + plugins
# Reconstruct ~/.marks directory

# Install apps:

# Adobe Lightroom
# Arq
# BBEdit
# Google Chrome
# Kaleidoscope
# Karabiner
# Launchbar
# Little Snitch
# Name Mangler (Mac App Store)
# Omnifocus
# Pixelmator (Mac App Store)
# QuickTime Player 7
# Shortcat
# Slack
# Slate
# SuperDuper
# Transmit
# iA Writer
# iWork

# Install fonts from server
