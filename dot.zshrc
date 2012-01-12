bindkey -e


### history
setopt extended_history
setopt hist_expand
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt hist_no_store
setopt hist_ignore_space
setopt share_history
setopt inc_append_history

HISTFILE="$HOME/.zhistory"
HISTSIZE=100000
SAVEHIST=100000


### prompt
unsetopt promptcr
setopt prompt_subst
autoload -U colors; colors
autoload -Uz vcs_info

local HOSTNAME_COLOR=$'%{\e[38;5;190m%}'
local USERNAME_COLOR=$'%{\e[38;5;199m%}'
local PATH_COLOR=$'%{\e[38;5;61m%}'
local RUBY_COLOR=$'%{\e[38;5;31m%}'
local VCS_COLOR=$'%{\e[38;5;248m%}'

zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b] (%a)'

zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' unstagedstr '¹'  # display ¹ if there are unstaged changes
zstyle ':vcs_info:git:*' stagedstr '²'    # display ² if there are staged changes
zstyle ':vcs_info:git:*' formats '[%b]%c%u'
zstyle ':vcs_info:git:*' actionformats '[%b|%a]%c%u'

# ruby version
function ruby_prompt {
    result=`rbenv version | sed -e 's/ .*//'`
    if [ "$result" ] ; then
        echo "[$result]"
    fi
}

precmd () {
    psvar=()
    vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

RUBY_INFO=$'%{$RUBY_COLOR%}$(ruby_prompt)%{${reset_color}%}'
RPROMPT="${RUBY_INFO}%{${reset_color}%}"
PROMPT=$'%{$fg[yellow]%}%n%{$fg[red]%}@$fg[green]%}%m %{$fg[cyan]%}%~ %1(v|%F{green}%1v%f|)\n%{$fg[green]%}%#%{$reset_color%} '


### completion
autoload -Uz compinit; compinit -u

setopt auto_list
setopt auto_menu
setopt list_packed
setopt list_types
setopt print_eight_bit
setopt complete_in_word
setopt mark_dirs
setopt auto_param_slash
setopt auto_param_keys
setopt magic_equal_subst

_cache_hosts=(`perl -ne  'if (/^([a-zA-Z0-9.-]+)/) { print "$1\n";}' ~/.ssh/known_hosts`)
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' verbose yes
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([%0-9]#)*=0=01;31'
zstyle ':completion:*' list-colors $LS_COLORS


### etc
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

setopt no_unset
setopt auto_cd
setopt no_beep

setopt extended_glob
setopt numeric_glob_sort
setopt glob_complete
setopt glob_dots
setopt brace_ccl

setopt ignore_eof
setopt pushd_ignore_dups
setopt interactive_comments
setopt auto_pushd
setopt long_list_jobs
setopt short_loops
setopt transient_rprompt

setopt no_hup
setopt correct

# bindkey
bindkey '^P' history-beginning-search-backward
bindkey '^N' history-beginning-search-forward
bindkey '^X^F' forward-word
bindkey '^X^B' backward-word
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward

autoload zmv
alias zmv='noglob zmv -W'

# replace
autoload -U replace-string
zle -N replace-string

# insert previous command last word
autoload smart-insert-last-word
zle -N insert-last-word smart-insert-last-word
zstyle :insert-last-word match \
  '*([^[:space:]][[:alpha:]/\\]|[[:alpha:]/\\][^[:space:]])*'
bindkey '^]' insert-last-word

# quote previous word in single or double quote
autoload -U modify-current-argument
_quote-previous-word-in-single() {
    modify-current-argument '${(qq)${(Q)ARG}}'
    zle vi-forward-blank-word
}
zle -N _quote-previous-word-in-single
bindkey '^[s' _quote-previous-word-in-single

_quote-previous-word-in-double() {
    modify-current-argument '${(qqq)${(Q)ARG}}'
    zle vi-forward-blank-word
}
zle -N _quote-previous-word-in-double
bindkey '^[d' _quote-previous-word-in-double

# git completion
autoload -U bashcompinit
bashcompinit
export COMP_WORDBREAKS=
source /usr/local/etc/bash_completion.d/git-completion.bash

source ~/.aliases

# init rbenv
eval "$(rbenv init -)"
if [[ -f ~/.rbenv/completions/rbenv.zsh ]]; then
   source ~/.rbenv/completions/rbenv.zsh
fi
