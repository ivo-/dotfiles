export PATH="$HOME/.rbenv/bin:$PATH:$HOME/bin:/usr/local/heroku/bin"
export EDITOR="emacsclient -t"

source ~/bash/functions
source ~/bash/alias

# If not running interactively, that's enough
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

eval "$(rbenv init -)"
