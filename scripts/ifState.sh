#!/bin/sh
CHECKIF=$HOME/config/scripts/checkIfs.sh
INTs=$( $CHECKIF )
for INT in $INTs
do
  case $INT in
    w* ) 
      ESSID=$( iwconfig $INT | grep -oE 'ESSID:[[:alnum:]"-\_]*' )
      # echo -e "\033[0;32m$ESSID\033[0m"
      echo $ESSID
      ;;
    * )
      ADDR=$( ip addr show $INT | grep -oE 'inet [[:digit:].]*' )
      echo $ADDR ;;
  esac
done

