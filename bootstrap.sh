# Use this document to setup a new Mac.
# This is not meant to be run as a shell-script, rather, as a guide.

# Sign in to iCloud
# Sign in to iTunes Match

# Check for software updates
# Install ad blocking
# Change caps-lock to control

# Copy files from backup
# Install ~/code/other-repos from backup
# pull-down your dotfiles, or copy from backup

# Change your shell to zsh
chsh -s /bin/zsh

# Installs the developer command line tools
xcode-select --install

# Add a welcome message to your terminal
sudo echo "Welcome to UNIX!" > /etc/motd

# Set up ssh keys

# install homebrew - worth checking homepage for, potentially, updated instructions
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew install emacs --with-cocoa
brew install fzf
brew install git
brew install ispell
brew install pandoc
brew install par
brew install tree
brew install vim

# Install princeXML
# make install textplay (symlink)

# Karabiner (and a few other things) require this
mkdir ~/.config

# Symlink a bunch of things - run from $HOME
ln -s ~/code/dotfiles/git/gitconfig .gitconfig;
ln -s ~/code/dotfiles/git/gitignore_global .gitignore_global;
ln -s ~/code/dotfiles/vim .vim;
ln -s ~/code/dotfiles/emacs ~/.emacs.d;
ln -s ~/code/dotfiles/hammerspoon ~/.hammerspoon;
ln -s ~/code/dotfiles/zshrc ~/.zshrc
ln -s ~/code/dotfiles/zshenv ~/.zshenv
ln -s ~/code/dotfiles/DefaultKeyBinding.dict ~/Library/KeyBindings/

# Install vim plugin manager + plugins
# Reconstruct ~/.marks directory

# Install apps:

# Adobe Lightroom
# Arq
# BBEdit
# Dropbox
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
