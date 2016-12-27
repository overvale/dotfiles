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

brew install vim
brew install fzf
brew install par
brew install git
brew install tree
brew install ranger
brew install pandoc
brew install zsh
brew install ispell
brew install emacs --with-cocoa

# Install ~/code/other-repos from backup

# Install princeXML
# make install textplay (symlink)

# pull-down your dotfiles, or copy from backup

mkdir ~/.config

# Symlink a bunch of things - run from $HOME
ln -s ~/code/dotfiles/git/gitconfig .gitconfig;
ln -s ~/code/dotfiles/git/gitignore_global .gitignore_global;
ln -s ~/code/dotfiles/zsh .zsh;
ln -s ~/code/dotfiles/zsh/zshrc .zshrc;
ln -s ~/code/dotfiles/vim .vim;
ln -s ~/code/dotfiles/emacs ~/.emacs.d;
ln -s ~/code/dotfiles/hammerspoon ~/.hammerspoon;

# Install vim plugin manager + plugins
# Reconstruct ~/.marks directory

# Install dropbox

# Install apps:

# Adobe Lightroom
# Arq
# BBEdit
# Emacs
# Google Chrome
# Hammerspoon
# Name Mangler (Mac App Store)
# Pixelmator (Mac App Store)
# QuickTime Player 7
# Slack
# SuperDuper
# Transmit

# Install fonts from server
