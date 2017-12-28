#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


. ~/.config/i3/wallpaper.sh

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
export VISUAL="emacs-nox"
