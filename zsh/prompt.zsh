# Enable what needs to be enabled
setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git 
zstyle ':vcs_info:*' use-simple true
#zstyle ':vcs_info:*' check-for-changes true
precmd () { vcs_info }

# display (git) if dir is repo
zstyle ':vcs_info:*' formats ' %F{8}(git)'

# actionformats are used when something is happening in
# the repo, like a merge or rebase.
# %a = action identifier
zstyle ':vcs_info:*' actionformats '(%a)'

# Setup the variables used in the prompt
if [ -n "$SSH_CLIENT" ]; then local myUsermachine='%F{8}%n@%m:%f'
fi
myDir='%F{12}%~%f'
myBackgroundjobs='%F{1}%(1j. (%j jobs).)%f'
myPrompt='%F{9}❯ %f'

# The prompt itself
PROMPT='
${myUsermachine}${myDir}${myBackgroundjobs}${vcs_info_msg_0_}
${myPrompt}'
