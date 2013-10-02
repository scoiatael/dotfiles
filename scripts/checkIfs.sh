#!/bin/sh
DEVPATH=/sys/class/net
INTs=$( ls $DEVPATH )
for INT in $INTs 
do
#    echo "Checking for $INT.."
    if [[ -e $DEVPATH/$INT/operstate ]] 
    then 
      STATE=$( cat $DEVPATH/$INT/operstate ) 
    fi
    if [[ X$STATE == Xup ]] 
    then
      echo $INT
    fi
done

