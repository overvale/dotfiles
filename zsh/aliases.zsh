# Alias Editing
alias reload='source $DOT/**/*.zsh'
alias ea='bbedit -w $DOT/zsh/aliases.zsh'
# personal aliases (not for distribution)
alias epa='bbedit -w ~/.zsh/localrc.zsh && reload'


# Suffix Aliases (zsh)
alias -s txt=bbedit
alias -s text=bbedit
alias -s md=bbedit


# Moving around & using the file-system
# ----------------------------------------------------------------------

# rm protection
alias rm="rm -i"

# mv overwrite protection
alias mv="mv -i"

# ls
alias l='ls -1p'         # my favorite options
alias la="ls -AohGp"    # long list, all, colors

# cd
alias ..="cd .."
alias cdb="cd -"

# open $PWD in Finder
alias o='open .'

# Create directory and cd into it
function cake() {
mkdir -p "$@" && cd "$@"
}

# cd to the front-most finder window's dir - no error handling
alias cdf='cd $(osascript -e "tell application \"Finder\" to POSIX path of (target of window 1 as alias)")'

# copy the working directory path
alias cpwd='pwd|tr -d "\n"|pbcopy'

# copy the file-path
function cfp(){
echo "$PWD/$1"|pbcopy
echo "File-Path is on the clipboard"
}

# Size of file/directory
function size(){
  du -sh "$1"
}

# show directory and file tree
function tree(){
find ./ -print | awk -F "/" '{for (i=1; i<=NF-2; i++){printf "|  "} print "|--"$NF}'
}

# quick exit
alias xit=exit

# Working with files
# ----------------------------------------------------------------------

# quicklook
alias ql='qlmanage -p "$@" >& /dev/null'

# make executable
alias ax="chmod a+x"

# Move to Trash - remove to trash
function rt(){
mv $1 ~/.Trash;
}


# System Info & Tools
# ----------------------------------------------------------------------

# What's the battery status?
alias batt="pmset -g batt"

# What's my I.P. Address?
# this looks for the ip address of "en0" - see `ipconfig` for more
myip() { (awk '{print $2}' <(ifconfig en0 | grep 'inet ')); }

# Quick way to rebuild the Launch Services database and get rid
# of duplicates in the Open With submenu.
alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user; killall Finder'


# Notes
# ----------------------------------------------------------------------

function note(){
  if [ "$1" = "" ] ; then
    exec $EDITOR ~/Dropbox/notes/note_`date +"%Y%m%d_%H%M%S"`.txt
  else
    exec $EDITOR ~/Dropbox/notes/"$1".txt
  fi
}

alias notes="open ~/Dropbox/notes/;exit"


# Miscellaneous
# ----------------------------------------------------------------------

# Arq has a command-line tool!
function arq(){
  /Applications/Arq.app/Contents/MacOS/Arq "$1"
}

# Lookup in Mac's dictionary
function dict(){
open dict:///"$1"
}

# Wolfram Alpha
function wolf(){
open "http://www.wolframalpha.com/input/?i=$*"
}

# Should I watch this movie?
function movie-info(){
open -a Safari "http://www.youtube.com/results?search_query=$1+trailer" "http://www.rottentomatoes.com/search/?search=$1" "http://www.imdb.com/find?q=$1"
}

