# Alias Administration
# ----------------------------------------------------------------------

# Alias Editing
alias reload='source $DOT/**/*.zsh'
alias ea='bbedit -w $DOT/zsh/aliases.zsh && reload'

# Suffix Aliases (zsh)
alias -s txt=bbedit
alias -s text=bbedit
alias -s md=bbedit


# Moving around & using the file-system
# ----------------------------------------------------------------------

# ls
alias l='ls -1p'         # my favorite options
alias la="ls -AohGp"    # long list, all, colors

# cd
alias ..="cd .."
alias cdb="cd -"

# open $PWD in Finder
alias o='open . &'

# directories
alias md='mkdir -p'

# Create directory and cd into it
function cake() {
mkdir -p "$1";
cd "$1"
}


# Utilities
# ----------------------------------------------------------------------

# PROCESSES
alias tu='top -o cpu' # cpu

# quicklook
alias ql='qlmanage -p "$@" >& /dev/null'       # quick view file

# make executable
alias ax="chmod a+x"

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

# Move to Trash
function trash(){
mv $1 ~/.Trash;
}

# What's my I.P. Address?
# this looks for the ip address of "en1" - see `ipconfig` for more
myip() { (awk '{print $2}' <(ifconfig en0 | grep 'inet ')); }


# Quick way to rebuild the Launch Services database and get rid
# of duplicates in the Open With submenu.
alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user; killall Finder'

# Airport On/Off - usage: "wifi off"
# to get wifi status: networksetup -getairportpower
function wifi(){
networksetup -setairportpower en0 $1;
}

# Lookup in Mac's dictionary
function dict(){
open dict:///"$1"
}

# Send to QC (https://github.com/danielcorin/qc.git)
function math(){
/Users/oliver/code/qc/qc/qc.sh "$1" | sed s/\n//g | pbcopy | echo "\"`pbpaste`\" is on the clipboard"
}

# mute the system volume
alias stfu="osascript -e 'set volume output muted true'"

# Terminal-Notifier
function notify(){
~/code/bin/terminal-notifier.app/Contents/MacOS/terminal-notifier -message "$1";
}

# TerminaIMdB
alias imdb="/Users/oliver/code/terminaimdb/terminalmdb.py"

# google
function google(){
open -a Safari "http://www.google.com/search?query=$1"
}

# wolfram alpha
function wolframalpha(){
open -a Safari "http://www.wolframalpha.com/input/?i=$1&appid=XWRP9J-6XWG83LEPE"
}

# Should I watch this movie?
function movie-info(){
open -a Safari "http://www.youtube.com/results?search_query=$1+trailer" "http://www.rottentomatoes.com/search/?search=$1" "http://www.imdb.com/find?q=$1"
}

# What movies are playing?
alias movies="open 'http://goo.gl/wH1lc' 'http://goo.gl/Unr0l'"
