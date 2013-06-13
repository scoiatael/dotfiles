#!/bin/bash

function usage {
  echo " Usage: $0 [init] "
}

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPOSCRIPT="$DIR/scripts/parselist"
REPODIR="$DIR/repos"

echo $REPOSCRIPT

function start {
  case $1 in
    "update" )
      for REPO in $( $REPOSCRIPT path )
      do
        echo "Updating $REPO.."
        cd $REPODIR/$REPO && echo $( git pull origin master )
      done ;; 
    "init" )
      for REPO in $( $REPOSCRIPT url )
      do
        echo "Cloning $REPO.."
        cd $REPODIR && echo "$( git clone $REPO )"
      done ;; 
    * )
      echo "Bad args.."
      usage ;;
  esac 
}

case $@ in
  "" )
    start update;;
  "--help" )
    usage ;;
  *)
    start $1 ;;
esac

