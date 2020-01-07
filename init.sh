#!/bin/bash -eux

# Shell scriptは成るべくPOSIX互換に書く - .｡oO(さっちゃんですよヾ(〃l _ l)ﾉﾞ☆) https://scrapbox.io/ne-sachirou/Shell_script%E3%81%AF%E6%88%90%E3%82%8B%E3%81%B9%E3%81%8FPOSIX%E4%BA%92%E6%8F%9B%E3%81%AB%E6%9B%B8%E3%81%8F

function install_ansible_by_apt() {
  $(with_sudo apt) update
  $(with_sudo apt) install ansible
}

function install_ansible_by_brew() {
  brew update
  brew install ansible
}

function with_sudo() {
  if test "" = "$(find "$(command -v "$1")" -user root)" ; then
    echo "$1"
  else
    echo "sudo $1"
  fi
}

case $(uname -s) in
  Darwin*)
    if ! command -v brew &> /dev/null ; then
      /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
    install_ansible_by_brew
    ;;
  Linux*)
    install_ansible_by_apt
    ;;
esac
mkdir -p ~/dotfiles
git clone --depth=1 git@github.com:ne-sachirou/dotfiles.git ~/dotfiles
