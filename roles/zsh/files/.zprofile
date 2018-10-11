# zmodload zsh/zprof && zprof

PATH=$HOME/.local/bin:$PATH
export PATH

eval "$(direnv hook zsh)"
eval "$(helm completion zsh)"
eval "$(pipenv --completion)"
