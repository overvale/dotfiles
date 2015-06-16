#  # My prompt is setup with git info
#  #
#  # Step one is to enable vcs_info and tell it how to format the
#  # information you want in the prompt
#  # 
#  setopt prompt_subst
#  autoload -Uz vcs_info
#  zstyle ':vcs_info:*' stagedstr ' Δ'
#  zstyle ':vcs_info:*' unstagedstr ' Δ'
#  zstyle ':vcs_info:*' check-for-changes true
#  zstyle ':vcs_info:*' actionformats '[%b|%a]'  #[branch|action]
#  if [[ $THEME == "light" ]]; then
#      zstyle ':vcs_info:*' formats '%F{4}git:%b%m%f%F{2}%c%f%F{5}%u%f'     #(<branch> <ahead|behind>) stagedstr unstagedstr
#  else
#      zstyle ':vcs_info:*' formats '%F{4}git:%b%m%f%F{2}%c%f%F{3}%u%f'     #(<branch> <ahead|behind>) stagedstr unstagedstr
#  fi
#  zstyle ':vcs_info:git*+set-message:*' hooks git-untracked git-aheadbehind
#  zstyle ':vcs_info:*' enable git 
#  
#  # This shows a marker if there are untracked files in the repo
#  +vi-git-untracked() {
#    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
#      git status --porcelain | grep '??' &> /dev/null ; then
#      hook_com[unstaged]+=' %F{1}+%f'
#    fi  
#  }
#  
#  ### git: Show +N/-N when your local branch is ahead-of or behind remote HEAD.
#  # Make sure you have added misc to your 'formats':  %m
#  function +vi-git-aheadbehind() {
#      local ahead behind
#      local -a gitstatus
#  
#      ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l | sed 's/ //g')
#      (( $ahead )) && gitstatus+=( " ↑${ahead}" )
#  
#      behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l | sed 's/ //g')
#      (( $behind )) && gitstatus+=( " ↓${behind}" )
#  
#      hook_com[misc]+=${(j::)gitstatus}
#  }
#  
#  # And here's the actual prompt
#  precmd () { vcs_info }
#  if [[ $THEME == "light" ]]; then
#  PROMPT='
#  %K{1}%F{11}%(1j.%jj .)%f%k%K{10}%F{2}[%~]%f%k ${vcs_info_msg_0_}
#  %F{1}$ %f'
#  else
#  PROMPT='
#  %F{1}%(1j.%jj .)%f[%F{2}%~%f] ${vcs_info_msg_0_}
#  %F{1}$ %f'
#  fi

# Same thing, but without the git info
if [[ $THEME == "light" ]]; then
PROMPT='
%K{11}%F{1}%(1j.%jj .)%f%k%F{8}[%~]%f
   %F{1}$%f'
else
PROMPT='
%F{1}%(1j.%jj .)%f[%F{2}%~%f]
   %F{1}$%f'
fi
