setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data
setopt HIST_IGNORE_SPACE
setopt APPEND_HISTORY       # write history only when closing
setopt EXTENDED_HISTORY     # add more info
setopt NOCLOBBER            # disallow overwriting of files using >

HISTFILE=~/.zsh_history
SAVEHIST=10000
HISTSIZE=10000

# Setting for the new UTF-8 terminal support in Lion
LC_CTYPE=en_US.UTF-8
LC_ALL=en_US.UTF-8

export PATH=/usr/local/bin:$PATH:~/code/dotfiles/bin:~/code/text-utilities
export EDITOR=vim
export DOT=~/code/dotfiles
export FZF_DEFAULT_OPTS="--color=light"

# enable completion
autoload -U compinit
compinit -C

# don't expand aliases _before_ completion has finished
setopt complete_aliases

# ignore case for tab completion
set completion-ignore-case on

# show all suggestions after pressing tab once instead of twice
set show-all-if-ambiguous on

# enable spelling correction
setopt CORRECT
setopt CORRECTALL

## case-insensitive (all),partial-word and then substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' \
    'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# pasting with tabs doesn't perform completion
zstyle ':completion:*' insert-tab pending

bindkey "^K" history-beginning-search-backward
bindkey "^J" history-beginning-search-forward
alias rm="rm -i"        # overwrite protection
alias mv="mv -i"        # overwrite protection
alias l='ls -1p'        # my favorite options
alias la="ls -AohGp"    # long list, all, colors
alias ..="cd .."
alias ax="chmod a+x"
alias cpwd='pwd|tr -d "\n"|pbcopy' # copy pwd
alias tmn='tmux new -s `basename $PWD`'
alias tma='tmux attach -t '
alias tml='tmux list-sessions'
alias tmk='tmux kill-session -t '
function size(){ du -sh "$1" }
function rt(){ mv $1 ~/.Trash; }
alias cdf='cd $(osascript -e "tell application \"Finder\" to POSIX path of (target of window 1 as alias)")'
function cfp(){ echo "$PWD/$1"|pbcopy && echo "File-Path is on the clipboard" }
alias batt="pmset -g batt"
myip() { (awk '{print $2}' <(ifconfig en0 | grep 'inet ')); }
alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'
alias server='python -m SimpleHTTPServer 8000'
alias unquarantine='xattr -d com.apple.quarantine'
