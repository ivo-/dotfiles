
#### FIG ENV VARIABLES ####
# Please make sure this block is at the start of this file.
[ -s ~/.fig/shell/pre.sh ] && source ~/.fig/shell/pre.sh
#### END FIG ENV VARIABLES ####
# Luke's config for the Zoomer Shell

# Enable colors and change prompt:
autoload -U colors && colors
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history
unsetopt share_history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.


export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t"

# ------------------------------------------------------------------------------
# Aliases

# Basic
alias c='clear'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias ll='ls -lah'
alias grep='grep --color=auto'
alias gerp='grep --color=auto'

# Open the current directory in a Finder window
alias ofd='open $PWD'

# Emacs
alias e='emacsclient -t'
alias ec='emacsclient -c'
alias E='SUDO_EDITOR="emacsclient -t -a emacs" sudoedit'
alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nw"

# Git
alias g='git'
alias ga='g add'
alias gp='g push'
alias gs='g status'
alias gb='g branch'
alias gc='g commit -m'
alias gca='g commit -am'
alias gch='g checkout'
alias glt='g log -n 10'
alias glp='g log --graph --decorate --pretty=oneline --abbrev-commit --all'

# Folders
alias .dw='cd ~/Downloads ; ll'
alias .dp='cd ~/Downloads ; ll'
alias .pj='cd ~/Projects ; ll'

# ------------------------------------------------------------------------------
# Plugins

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh # brew install zsh-syntax-highlighting
source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh # brew install zsh-autosuggestions

# brew install pure
autoload -U promptinit; promptinit
prompt pure

# change the path color
zstyle :prompt:pure:path color green

# change the color for both `prompt:success` and `prompt:error`
zstyle ':prompt:pure:prompt:*' color red
export PATH="/usr/local/opt/libpq/bin:$HOME/go/bin/:$PATH"

#### FIG ENV VARIABLES ####
# Please make sure this block is at the end of this file.
[ -s ~/.fig/fig.sh ] && source ~/.fig/fig.sh
#### END FIG ENV VARIABLES ####
