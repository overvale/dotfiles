# Use this document to setup a new Mac.
# This is not meant to be run as a shell-script,
# rather, as a guide.

# Sign in to iCloud
# Sign in to iTunes Match

# Change caps-lock to control

# Installs the developer command line tools
xcode-select --install

# set default shell
chsh -s /bin/zsh

# install homebrew - worth checking hompage
# for, potentially, updated instructions
ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

brew install vim
brew install par
brew install git
brew install tree
brew install tmux
brew install reattach-to-user-namespace
curl -O http://sreitshamer.github.io/arq_restore/arq_restore.zip && unzip arq_restore.zip && rm arq_restore.zip
pip install awscli

# Install Vundle (for vim)
# Install vim plugins

# Set Help Viewer windows to non-floating mode
defaults write com.apple.helpviewer DevMode -bool true

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Install apps:
# Adobe Lightroom
# BBEdit
# Dropbox
# Google Chrome
# iTerm
# Kaleidoscope
# Karabiner
# Keyboard Maestro
# Name Mangler
# OmniFocus
# Patterns
# Pixelmator
# Slack
# SuperDuper
# The Unarchiver
# Transmit
# QuickTime Player 7

# pull-down your dotfiles; via Dropbox or git

# Create centralized vim swp files location
mkdir ~/.swp-dir

# Symlink a bunch of things - run from $HOME
ln -s ~/code/dotfiles/git/gitconfig .gitconfig
ln -s ~/code/dotfiles/git/gitignore_global .gitignore_global
ln -s ~/code/dotfiles/zsh .zsh
ln -s ~/code/dotfiles/zsh/zshrc .zshrc
ln -s ~/code/dotfiles/vim .vim
ln -s ~/code/dotfiles/vim/vimrc .vimrc
ln -s code/dotfiles/vim/gvimrc .gvimrc
ln -s ~/code/dotfiles/Karabiner ~/Library/Application\ Support/Karabiner
ln -s code/dotfiles/tmux.conf .tmux.conf

# install Arq
# http://www.haystacksoftware.com/arq/
# and restore anything that wasn't automatically restored by
# cloud services (iCloud, Dropbox, etc.)

