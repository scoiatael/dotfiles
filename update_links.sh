#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LINKDIR="$DIR/dotfiles"

for LINKFILE in $( ls $LINKDIR/*.link ) 
do
	LINKPATH=$DIR/$( cat $LINKFILE )
	LINKNAME=${LINKFILE%.link}
  rm -fr $HOME/.$( basename $LINKNAME )
  ln -fvs $LINKPATH $HOME/.$( basename $LINKNAME )
done
