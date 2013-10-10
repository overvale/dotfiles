#!/bin/zsh

# Use this script to setup a new mac with your dot-files
# ensure you run it from $HOME

ln -s ~/code/dotfiles/git/gitconfig .gitconfig
ln -s ~/code/dotfiles/git/gitignore_global .gitignore_global

ln -s ~/code/dotfiles/zsh .zsh
ln -s ~/.zsh/zshrc .zshrc

ln -s ~/code/dotfiles/vim .vim
ln -s ~/.vim/vimrc .vimrc


# After you've installed KeyRemap4MacBook & Launchbar:
ln -s ~/code/dotfiles/KeyRemap4MacBook ~/Library/Application\ Support/KeyRemap4MacBook
# ln -s ~/code/lb_snippets ~/Library/Application\ Support/Launchbar/Snippets


# Software to install
# ===================
# 
# Arq
# BBEdit
# Keyboard Meastro
# KeyboardRemap4MacBook
# Lightroom
# Transmit


# A few personal preferences...
# Stolen from: https://github.com/mathiasbynens/dotfiles

# Set Help Viewer windows to non-floating mode
defaults write com.apple.helpviewer DevMode -bool true

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
