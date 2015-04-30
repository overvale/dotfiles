bindkey "^K" history-beginning-search-backward
bindkey "^J" history-beginning-search-forward
alias theme-dark='export THEME=dark && source $DOT/zsh/prompt.zsh'
alias theme-light='export THEME=light && source $DOT/zsh/prompt.zsh'
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
alias gitACP="git add -A && git commit -m 'updates' && git push"
alias batt="pmset -g batt"
myip() { (awk '{print $2}' <(ifconfig en0 | grep 'inet ')); }