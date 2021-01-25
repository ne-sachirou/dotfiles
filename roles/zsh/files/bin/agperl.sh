#!/bin/bash -eux
shellcheck "$0"
ag -l "$1" | xargs -I{} -t perl -pi -e"s/$1/$2/g" {}
