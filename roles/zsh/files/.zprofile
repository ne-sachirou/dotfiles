# zmodload zsh/zprof && zprof

PERL_BIN_DIR=$(cd $(dirname $(which perl)) && cd $(dirname $(readlink -n perl)) && pwd)
PATH=$HOME/.cargo/bin/:$HOME/.krew/bin:$HOME/.local/bin:$HOME/.poetry/bin:$HOME/go/bin:$PERL_BIN_DIR:$PATH
export PATH

eval "$(/opt/homebrew/bin/brew shellenv)"
eval "$(direnv hook zsh)"
# eval "$(helm completion zsh)"
# eval "$(pipenv --completion)"
