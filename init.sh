#!/bin/bash -eux

if ! $(which brew &> /dev/null) ; then
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
brew update
brew install ansible
mkdir -p ~/dotfiles
git clone --depth=1 git@github.com:ne-sachirou/dotfiles.git ~/dotfiles
