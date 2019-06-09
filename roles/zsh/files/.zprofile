# zmodload zsh/zprof && zprof

GOPATH=$HOME/dev/go
PATH=$HOME/.local/bin:$HOME/.cargo/bin/:$GOPATH/bin:$PATH
export GOPATH PATH

eval "$(direnv hook zsh)"
eval "$(helm completion zsh)"
eval "$(pipenv --completion)"
