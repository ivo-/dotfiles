#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Set keyboard layout options.
setxkbmap -layout us,bg
setxkbmap -variant ,phonetic
setxkbmap -option grp:shifts_toggle,terminate:ctrl_alt_bksp

# Load custom key map.
if [ -s ~/.Xmodmap ]; then
  xmodmap ~/.Xmodmap
fi

# Start network manager.
nm-applet --sm-disable &

# Start emacs daemon.
emacs --daemon --with-x-toolkit=lucid &

# Hide cursor when idle.
unclutter &

# Start redshift.
redshift-gtk &

# Start awesome.
exec awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
