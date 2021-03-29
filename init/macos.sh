#!/bin/bash

# Shell scriptは成るべくPOSIX互換に書く - .｡oO(さっちゃんですよヾ(〃l _ l)ﾉﾞ☆) https://scrapbox.io/ne-sachirou/Shell_script%E3%81%AF%E6%88%90%E3%82%8B%E3%81%B9%E3%81%8FPOSIX%E4%BA%92%E6%8F%9B%E3%81%AB%E6%9B%B8%E3%81%8F

set -eux

xcode-select --install
if ! command -v brew &> /dev/null ; then
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
brew update
brew install ansible
mkdir -p ~/dotfiles
git clone git@github.com:ne-sachirou/dotfiles.git ~/dotfiles
