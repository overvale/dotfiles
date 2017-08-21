# OPTIONS
# ----------------------------------------------------------------------------

shopt -s histappend
export HISTCONTROL=ignoreboth:erasedups
export HISTTIMEFORMAT="%Y/%m/%d %H:%M:%S:   "
SAVEHIST=10000
HISTSIZE=5000

set -o noclobber

export PATH=$PATH:~/code/dotfiles/bin:~/code/text-utilities:~/code/other-repos/fzf-fs
export EDITOR=vim
export DOT=~/code/dotfiles

export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# ALIASES
# ----------------------------------------------------------------------------

bind '"\e[A":history-search-backward'
bind '"\e[B":history-search-forward'

alias rm="rm -i"        # overwrite protection
alias mv="mv -i"        # overwrite protection
alias l='ls -1p'        # my favorite options
alias la="ls -AohGp"    # long list, all, colors
alias ..="cd .."
alias ax="chmod a+x"
rt(){ mv $1 ~/.Trash; }
alias cdf='cd $(osascript -e "tell application \"Finder\" to POSIX path of (target of window 1 as alias)")'
