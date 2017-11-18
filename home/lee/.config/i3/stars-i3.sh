#!/bin/bash

i="$(( ( RANDOM % 20 )  + 1 ))"
A="$(date +%S)"
if [ $[$A%2] -eq 0 ]; then
    echo "★"
else
    echo "☆"
fi
exit
