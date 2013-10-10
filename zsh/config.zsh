# History
# ----------------------------------------------------------------------
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data
setopt HIST_IGNORE_SPACE
setopt APPEND_HISTORY       # write history only when closing
setopt EXTENDED_HISTORY     # add more info

HISTFILE=~/.zsh_history
SAVEHIST=10000
HISTSIZE=10000
