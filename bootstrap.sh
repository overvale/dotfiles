# Use this document to setup a new Mac.
# This is not meant to be run as a shell-script, rather, as a guide.

# Sign in to iCloud
# Sign in to iTunes Match

# Check for software updates
# Install ad blocking
# Change caps-lock to control

# Copy files from backup
# Install ~/dev/other-repos from backup
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

brew install aspell
brew install emacs --with-cocoa
brew install fzf
brew install git
brew install ispell
brew install pandoc
brew install par
brew install the_silver_searcher
brew install tree
brew install vim

# Install princeXML
# make install textplay (symlink)

# Karabiner (and a few other things) require this
mkdir ~/.config

# Symlink a bunch of things - run from $HOME
ln -s ~/d/src/dot/vim .vim;
ln -s ~/dev/emacs ~/.emacs.d;
ln -s ~/d/src/dot/hammerspoon ~/.hammerspoon;
ln -s ~/d/src/dot/DefaultKeyBinding.dict ~/Library/KeyBindings/
ln -s ~/d/src/dot/git/gitconfig .gitconfig;
ln -s ~/d/src/dot/git/gitignore_global .gitignore_global;
ln -s ~/d/src/dot/zshrc ~/.zshrc
ln -s ~/d/src/dot/zshenv ~/.zshenv
ln -s ~/d/src/dot/bashrc ~/.bashrc
ln -s ~/d/src/dot/bashrc ~/.bash_profile

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
