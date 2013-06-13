#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LINKDIR="$DIR/dotfiles/"

for LINKFILE in $( ls $LINKDIR ) 
do
	LINKPATH=$DIR/$( cat $LINKDIR/$LINKFILE )
	LINKNAME=${LINKFILE%.link}
	rm -fr $HOME/.$LINKNAME
	ln -fvs $LINKPATH $HOME/.$LINKNAME
done
