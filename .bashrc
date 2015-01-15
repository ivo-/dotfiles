export PATH="$HOME/.rbenv/bin:$PATH:$HOME/bin:/usr/local/heroku/bin"
export EDITOR="emacsclient -t"

source ~/bash/functions
source ~/bash/alias

# Show my simple ref-card
ref ()
{
    echo `cat bash/ref`
}

# If not running interactively, that's enough
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

eval "$(rbenv init -)"
