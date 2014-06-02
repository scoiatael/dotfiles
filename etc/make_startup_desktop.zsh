#!/bin/zsh
echo "
[Desktop Entry]
Name=Utilities autostart 
Exec=$HOME/.xmonad/startup.sh
Icon=rxvt-unicode
Type=Application
Categories=Utility
" > $HOME/.config/autostart/startup.desktop
