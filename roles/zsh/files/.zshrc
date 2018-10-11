# zmodload zsh/zprof && zprof

fpath=(~/.zsh $fpath)

# {{{ init
bindkey -e
zstyle :compinstall filename $HOME/.zshrc
autoload -Uz compinit
# }}} init

# {{{ zplug
export ZPLUG_HOME=$HOME/.zplug
source $ZPLUG_HOME/init.zsh
zplug "mollifier/anyframe"
zplug "b4b4r07/enhancd", use:init.sh
# }}} zplug

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

. ~/.zsh/assh_autocomplete.zsh
. ~/.zsh/git-prompt.sh

HISTFILE=~/.bash_history
HISTSIZE=10000
SAVEHIST=100000
export EDITOR='vim'
export LESS='-iMR'
export PAGER='less -X'

alias be='bundle exec'
alias ssh="assh wrapper ssh"
alias pv=private-values
alias vi='emacsclient -nw'

_cache_hosts=($(assh config list | perl -waln -F'\->' -e 'if(/->/){$F[0]=~s/^\s*(.*?)\s*$/$1/;print$F[0]}'))

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
setopt PROMPT_SUBST
setopt TRANSIENT_RPROMPT
precmd () {
  PROMPT="%F{red}%n%f%F{blue}@%f%F{red}%M%f%F{blue} %?%#%f"
  RPROMPT="%F{blue}[%f%F{red}%~$(__git_ps1)%f %F{blue}%*]%f"
}
# }}} prompt

# {{{ cd
function list_status() {
  if [ -n "$BUFFER" ]; then
    zle accept-line
    return 0
  fi
  echo
  ls -FG
  if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = 'true' ]; then
    echo
    echo -e "\e[0;33m--- git status ---\e[0m"
    git status -sb
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
  list_status
}
if [ -e /tmp/pwd_11a37b13f64c46bfb5a0282279e6bb38 ]; then
  cd $(cat /tmp/pwd_11a37b13f64c46bfb5a0282279e6bb38)
fi
# }}} cd

. ~/.asdf/asdf.sh
. ~/.asdf/completions/asdf.bash

case "${OSTYPE}" in
darwin*)
  source ~/.zsh/.zshrc.darwin
  ;;
linux*)
  source ~/.zsh/.zshrc.linux
  ;;
esac

zplug load

# if [ $(expr $(date +%s) / 86400) != $(expr $(stat --format '%Y' ~/.zcompdump) / 86400) ]; then
#   compinit
# else
#   compinit -C
# fi
compinit -C

# if (which zprof > /dev/null) ;then
#   zprof
# fi

# vim:set fdm=marker:
