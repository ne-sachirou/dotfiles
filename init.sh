#!/bin/bash -eux

# Single UNIX Specification (SUSv3) Shell and Utilities (XCU) http://pubs.opengroup.org/onlinepubs/009696699/utilities/contents.html

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
