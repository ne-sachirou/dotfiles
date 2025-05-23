# zmodload zsh/zprof && zprof

# {{{ init
bindkey -v
zstyle :compinstall filename "$HOME/.zshrc"
autoload -Uz compinit
compinit
# }}} init

# {{{ zi
. ~/.zi/bin/zi.zsh
autoload -Uz _zi
(( ${+_comps} )) && _comps[zi]=_zi
zi ice src"init.sh"
zi light babarot/enhancd
# zi light jeffreytse/zsh-vi-mode # Similar to `bindkey -v`
zi light mollifier/anyframe
zi light momo-lab/zsh-abbrev-alias
zi light zsh-users/zsh-autosuggestions
zi light zsh-users/zsh-completions
zi light zsh-users/zsh-syntax-highlighting
# }}} zi

if [[ -a /usr/local/etc/bash_completion.d/git-prompt.sh ]]; then
  . /usr/local/etc/bash_completion.d/git-prompt.sh
else
  . ~/.zsh/git-prompt.sh
fi
. ~/.zsh/lazyenv.bash
. ~/git-subrepo/.rc

EDITOR='vim'
GOPATH=$HOME/go
HISTFILE=~/.bash_history
HISTSIZE=10000
LESS='-iMR'
MACKEREL_APIKEY="$(private-values get hatena.MACKEREL_APIKEY)"
PAGER='less -X'
SAVEHIST=100000
USE_GKE_GCLOUD_AUTH_PLUGIN=True
export EDITOR GOPATH HISTFILE HISTSIZE LESS MACKEREL_APIKEY OP_SESSION_hatena PAGER SAVEHIST USE_GKE_GCLOUD_AUTH_PLUGIN

abbrev-alias -g be='bundle exec'
abbrev-alias -g g=git
abbrev-alias -g k=kubectl
abbrev-alias -g pv=private-values

alias j='docker run -it -v "$(pwd):/mnt" --rm nesachirou/jlang'

alias vi='emacsclient -nw'
#function vi() {
#  if test "$(stat "$1" > /dev/null || echo $?)" ; then
#    echo -n "Create a file? (y/N): "
#    read yn
#    case "$yn" in
#      [yY]*) touch "$1"
#             emacsclient -nw "$1" ;;
#      *) : ;;
#    esac
#  else
#    emacsclient -nw "$1"
#  fi
#}

# {{{ history
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt share_history
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
# }}} history

# {{{ prompt
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUPSTREAM=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWDIRTYSTATE GIT_PS1_SHOWUPSTREAM GIT_PS1_SHOWUNTRACKEDFILES GIT_PS1_SHOWSTASHSTATE
setopt PROMPT_SUBST
setopt TRANSIENT_RPROMPT
precmd () {
  PROMPT="%F{blue}%*%f %F{red}%?%f%F{blue}%#%f "
  RPROMPT="%F{blue}[%f%F{red}%~$(__git_ps1)%f %F{blue}]%f"
  export PROMPT RPROMPT
}
# }}} prompt

# {{{ cd
function list_status() {
  if [ -n "$BUFFER" ]; then
    zle accept-line
    return 0
  fi
  echo
  ls -aFG
  if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = 'true' ]; then
    echo
    echo -e "\e[0;33m--- git status ---\e[0m"
    git -c color.status=always status -sb  --show-stash | head -n 20
  fi
}

function do_enter() {
  pwd > /tmp/pwd_11a37b13f64c46bfb5a0282279e6bb38
  list_status
  zle reset-prompt
}
zle -N do_enter
bindkey '^m' do_enter

function chpwd() {
  pwd > /tmp/pwd_11a37b13f64c46bfb5a0282279e6bb38
}
if [ -e /tmp/pwd_11a37b13f64c46bfb5a0282279e6bb38 ]; then
  cd "$(cat /tmp/pwd_11a37b13f64c46bfb5a0282279e6bb38)" || return
fi
# }}} cd

# {{{ anyframe
# bindkey '^r' history-incremental-search-backward # This is zsh default.
bindkey '^r' anyframe-widget-execute-history
# }}}

PATH=/nix/var/nix/profiles/default/bin:$PATH
. /nix/var/nix/profiles/default/etc/profile.d/nix.sh
export PATH

export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
fpath=(${ASDF_DATA_DIR:-$HOME/.asdf}/completions $fpath)

eval "$(direnv hook zsh)"

case "${OSTYPE}" in
darwin*)
  . ~/.zsh/.zshrc.darwin
  ;;
linux*)
  . ~/.zsh/.zshrc.linux
  ;;
esac

# if [ $(($(date +%s) / 86400)) != $(($(stat -f '%m' $HOME/.zcompdump) / 86400)) ]; then
#   compinit
# else
#   compinit -C
# fi

# zprof

# vim:set fdm=marker:
