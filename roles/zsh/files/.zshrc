# zmodload zsh/zprof && zprof

fpath=(~/.zsh $fpath)

# {{{ init
bindkey -e
zstyle :compinstall filename "$HOME/.zshrc"
autoload -Uz compinit
# }}} init

# {{{ zplug
export ZPLUG_HOME=$HOME/.zplug
. "$ZPLUG_HOME/init.zsh"
touch "$ZPLUG_LOADFILE"
zplug "b4b4r07/enhancd", use:init.sh
zplug "mollifier/anyframe"
zplug "zsh-users/zsh-autosuggestions"
# zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
# }}} zplug

. ~/.asdf/asdf.sh
. ~/.asdf/completions/asdf.bash
. ~/.zsh/lazyenv.bash
. ~/.zsh/git-prompt.sh

_ssh_init() {
  # _cache_hosts=($(assh config list | perl -waln -F'\->' -e 'if(/->/){$F[0]=~s/^\s*(.*?)\s*$/$1/;print$F[0]}'))
  . ~/.zsh/assh_autocomplete.zsh
}
eval "$(lazyenv.load _ssh_init assh ssh)"

HISTFILE=~/.bash_history
HISTSIZE=10000
SAVEHIST=100000
EDITOR='vim'
LESS='-iMR'
PAGER='less -X'
export HISTFILE HISTSIZE SAVEHIST EDITOR LESS PAGER

alias be='bundle exec'
alias j='docker run -it --rm nesachirou/jlang'
alias g=git
alias pv=private-values
alias ssh='assh wrapper ssh'
alias vi='emacsclient -nw'

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
  PROMPT="%F{red}%n%f%F{blue}@%f%F{red}%M%f%F{blue} %?%#%f"
  RPROMPT="%F{blue}[%f%F{red}%~$(__git_ps1)%f %F{blue}%*]%f"
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
}
if [ -e /tmp/pwd_11a37b13f64c46bfb5a0282279e6bb38 ]; then
  cd "$(cat /tmp/pwd_11a37b13f64c46bfb5a0282279e6bb38)" || return
fi
# }}} cd

# {{{ anyframe
bindkey '^xr' anyframe-widget-execute-history
bindkey '^x^r' anyframe-widget-execute-history
bindkey '^xk' anyframe-widget-kill
bindkey '^x^k' anyframe-widget-kill
# }}}

case "${OSTYPE}" in
darwin*)
  . ~/.zsh/.zshrc.darwin
  ;;
linux*)
  . ~/.zsh/.zshrc.linux
  ;;
esac

zplug load

if [ $(($(date +%s) / 86400)) != $(($(stat -f '%m' ~/.zcompdump) / 86400)) ]; then
  compinit
else
  compinit -C
fi

# zprof

# vim:set fdm=marker:
