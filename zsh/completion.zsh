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

