#!/bin/bash

mv ../dotfiles ../.dotfiles

packages="git vim zsh"

function apt_install {
	sudo apt update && sudo apt upgrade
	sudo apt install -y ${apt_packages}
}

apt_install
