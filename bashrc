# OPTIONS
# ----------------------------------------------------------------------------

# setopt SHARE_HISTORY        # share history data between sessions
# setopt complete_aliases           # don't expand aliases _before_ completion has finished
# set    completion-ignore-case on  # ignore case for tab completion
# set    show-all-if-ambiguous on   # show all suggestions after pressing tab once instead of twice

shopt -s histappend
export HISTCONTROL=ignoreboth:erasedups
export HISTTIMEFORMAT="%Y/%m/%d %H:%M:%S:   "
SAVEHIST=10000
HISTSIZE=5000

set -o noclobber

export PATH=$PATH:~/code/dotfiles/bin:~/code/text-utilities:~/code/other-repos/fzf-fs:/Applications/Racket\ v6.7/bin
export EDITOR=vim
export DOT=~/code/dotfiles

export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# PROMPT
# ----------------------------------------------------------------------------

# Setup the variables used in the prompt
if [ -n "$SSH_CLIENT" ]; then
    myUsermachine='\u@\h:'
fi
myDir='\w'
myPrompt='$ '
myJobs='\j '

PS1="\033[1;33m"${myUsermachine}"\033[0;32m"${myDir}"\033[0;34m"${myPrompt}"\033[0m"

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
alias cpwd='pwd|tr -d "\n"|pbcopy' # copy pwd
alias tmn='tmux new -s `basename $PWD`'
alias tma='tmux attach -t '
alias tml='tmux list-sessions'
alias tmk='tmux kill-session -t '
size(){ du -sh "$1"; }
rt(){ mv $1 ~/.Trash; }
alias cdf='cd $(osascript -e "tell application \"Finder\" to POSIX path of (target of window 1 as alias)")'
alias batt="pmset -g batt"
myip() { (awk '{print $2}' <(ifconfig en0 | grep 'inet ')); }
alias server='python -m SimpleHTTPServer 8000'
alias unquarantine='xattr -d com.apple.quarantine'
alias vvrc="vim $DOT/vim/vimrc"

alias gs='git status -sb'
alias gl='git lg'

# FUNCTIONS
# ----------------------------------------------------------------------------

function makepdf(){
    if test -z $3; then
        echo "USAGE: makepdf format /source /destination" >&2
        return 1
    else
        pandoc --smart -f $1 -t html < $2 | prince -s ~/code/print_css/print.css - -o $3
        return 0
    fi
}

function todo() {
    vim $(find ~/Dropbox/life.text ~/Documents/screenwriting/career/writing.txt ~/code/notes/code.text ~/Dropbox/ingenuity/ingenuity.text | fzf)
}
function gltodo(){
    grep TODO ~/Dropbox/life.text ~/Documents/screenwriting/career/writing.txt ~/code/notes/code.text ~/Dropbox/ingenuity/ingenuity.text | sed -E "s/(.+\/)(.+)(\..+)(TODO +)(.+)/\2: \5/g" | column -t -s :
}

# FZF OPTIONS / FUNCTIONS
# ----------------------------------------------------------------------------

source /usr/local/opt/fzf/shell/completion.bash
source /usr/local/opt/fzf/shell/key-bindings.bash
source ~/code/other-repos/fzf-marks/fzf-marks.plugin.bash

export FZF_DEFAULT_OPTS='--color dark'
export FZF_FS_OPENER=vim
alias ffs='fzf-fs'

# fd - cd to selected directory
fd() {
  DIR=`find ${1:-*} -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf-tmux` \
    && cd "$DIR"
}

# fshow - git commit browser
fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  local files
  IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# v - open files in ~/.viminfo
v() {
  local files
  files=$(grep '^>' ~/.viminfo | cut -c3- |
          while read line; do
            [ -f "${line/\~/$HOME}" ] && echo "$line"
          done | fzf-tmux -d -m -q "$*" -1) && vim ${files//\~/$HOME}
}

