# zmodload zsh/zprof && zprof

PATH=$HOME/.local/bin:$PATH
PATH=$HOME/.anyenv/bin:$PATH
PATH=/Library/Frameworks/Mono.framework/Versions/Current/bin:$PATH
export PATH

eval "$(direnv hook zsh)"
eval "$(helm completion zsh)"
eval "$(pipenv --completion)"
