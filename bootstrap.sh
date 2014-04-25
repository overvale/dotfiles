# Use this document to setup a new Mac.
# This is not meant to be run as a shell-script,
# rather, as a guide.

# Sign in to iCloud
# Sign in to iTunes Match

# Installs the developer command line tools
xcode-select --install

# set default shell
chsh -s /opt/local/bin/zsh

# install homebrew - worth checking hompage
# for, potentially, updated instructions
ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

brew install vim
brew install par
brew install git
curl -O http://sreitshamer.github.io/arq_restore/arq_restore.zip && unzip arq_restore.zip && rm arq_restore.zip

# Set Help Viewer windows to non-floating mode
defaults write com.apple.helpviewer DevMode -bool true

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Install apps:
# Adobe Lightroom
# BBEdit
# Dropbox
# Google Chrome
# iTerm2
# Kaleidoscope
# Keyboard Maestro
# KeyRemap4Macbook
# Launchbar
# Name Mangler
# Patterns
# Pixelmator
# SuperDuper
# The Unarchiver
# Transmit

# Symlink a bunch of things - run from $HOME
ln -s ~/code/dotfiles/git/gitconfig .gitconfig
ln -s ~/code/dotfiles/git/gitignore_global .gitignore_global
ln -s ~/code/dotfiles/zsh .zsh
ln -s ~/.zsh/zshrc .zshrc
ln -s ~/code/dotfiles/vim .vim
ln -s ~/.vim/vimrc .vimrc
ln -s ~/code/dotfiles/KeyRemap4MacBook ~/Library/Application\ Support/KeyRemap4MacBook

# install Arq
# http://www.haystacksoftware.com/arq/
# and restore anything that wasn't automatically restored by
# cloud services (iCloud, Dropbox, etc.)

