# My prompt is setup with git info
#
# Step one is to enable vcs_info and tell it how to format the
# information you want in the prompt
# 
setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:*' stagedstr ' Δ'
zstyle ':vcs_info:*' unstagedstr ' Δ'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' actionformats '[%b|%a]'  #[branch|action]
zstyle ':vcs_info:*' formats '%F{4}git:%b%m%f%F{2}%c%f%F{3}%u%f'     #(<branch> <ahead|behind>) stagedstr unstagedstr
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked git-aheadbehind
zstyle ':vcs_info:*' enable git 

# This shows a marker if there are untracked files in the repo
+vi-git-untracked() {
  if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
    git status --porcelain | grep '??' &> /dev/null ; then
    hook_com[unstaged]+=' %F{1}+%f'
  fi  
}

### git: Show +N/-N when your local branch is ahead-of or behind remote HEAD.
# Make sure you have added misc to your 'formats':  %m
function +vi-git-aheadbehind() {
    local ahead behind
    local -a gitstatus

    # for git prior to 1.7
    # ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l | sed 's/ //g')
    (( $ahead )) && gitstatus+=( " ↑${ahead}" )

    # for git prior to 1.7
    # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l | sed 's/ //g')
    (( $behind )) && gitstatus+=( " ↓${behind}" )

    hook_com[misc]+=${(j::)gitstatus}
}

# And here's the actual prompt
precmd () { vcs_info }
PROMPT='
%F{9}%(1j.%jj .)%f[%F{2}%~%f] ${vcs_info_msg_0_}
%F{1}$ %f'
