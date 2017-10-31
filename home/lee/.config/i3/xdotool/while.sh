#!/bin/bash
echo "script run"
while [ $SPACELOOP = open ]
do
    xdotool key space
    sleep 5

para1=$1
if [ ! -n $para1 ]; then  
  echo "IS NULL"  
else  
  echo "NOT NULL"  
fi 

done
