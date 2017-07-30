export PATH=$PATH:~/code/dotfiles/bin:~/code/text-utilities:~/code/other-repos/fzf-fs

function asp(){
    if test -z $1; then
        # Takes misspelled word as input, outputs suggestions from aspell
        aspell -a | tail -2 | sed 's/& .*: //' | awk 'gsub(/, /,"\n")'
        return 0
    else
        # Also takes 1st argument
        echo "$1" | aspell -a | tail -2 | sed 's/& .*: //' | awk 'gsub(/, /,"\n")'
        return 0
    fi
}

myip() { (awk '{print $2}' <(ifconfig en0 | grep 'inet ')); }
