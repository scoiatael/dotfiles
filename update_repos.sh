#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPOLIST="gitrepos.list"
CURDIR=$( pwd )

for REPO in $( cat $DIR/$REPOLIST )
do
	echo "Updating $REPO.."
	cd $DIR/$REPO
	echo $( pwd ) 
	echo $( git pull origin master )
done
cd $CURDIR
