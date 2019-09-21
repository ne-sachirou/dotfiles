# zmodload zsh/zprof && zprof

PERL_BIN_DIR=$(cd $(dirname $(which perl)) && cd $(dirname $(readlink -n perl)) && pwd)
PATH=$HOME/.local/bin:$HOME/.cargo/bin/:$PERL_BIN_DIR:$PATH
export PATH

eval "$(direnv hook zsh)"
# eval "$(helm completion zsh)"
# eval "$(pipenv --completion)"
