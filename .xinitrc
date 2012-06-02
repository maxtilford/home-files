#!/bin/zsh
xsetroot -solid "#3d659c"
xsetroot -cursor_name left_ptr
urxvtd -q -o -f
xset +fp /usr/share/fonts/local
xset fp rehash
dunst -to 3 -geometry 1200x500+125+400 -fn "-*-*-medium-r-*-*-40-*-*-*-*-*-*-*" &
xmonad

