# zmodload zsh/zprof && zprof

PATH=$HOME/.anyenv/bin:$PATH
export PATH

eval "$(anyenv init - --no-rehash)"
eval "$(direnv hook zsh)"
