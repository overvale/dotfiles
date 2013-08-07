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
ln -s ~/code/lb_snippets ~/Library/Application\ Support/Launchbar/Snippets


# Software to install
# ===================
# 
# Arq
# BBEdit
# Keyboard Meastro
# KeyboardRemap4MacBook
# Launchbar
# Lightroom
# Transmit
# iA Writer
