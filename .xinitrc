#!/bin/zsh
xsetroot -solid "#002b36"
xsetroot -cursor_name left_ptr
urxvtd -q -o -f
xset +fp /usr/share/fonts/local
xset fp rehash
dunst -to 3 -geometry 1200x500+125-100 -fn "-*-*-medium-r-*-*-40-*-*-*-*-*-*-*" &
if [ -f $HOME/.Xdefaults ]; then
  xrdb -merge $HOME/.Xdefaults
fi
vmware-user-suid-wrapper
xmonad
