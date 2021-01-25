#!/bin/bash -eux
shellcheck "$0"
rm -vf TAGS
# ag -g "${1?\\.(c|cpp|erl|h|hrl|java|php|pl|py|rb|tex|y)$}" | xargs -t etags -a
ag -g '\.(c|cpp|erl|h|hrl|java|php|pl|py|rb|tex|y)$' | xargs -t etags -a
