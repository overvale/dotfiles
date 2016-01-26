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
