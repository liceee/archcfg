#!/bin/bash

xdotool mousemove 1760 28 click 3
sleep 0.1
xdotool mousemove 1775 42 click 3
sleep 0.1
xdotool mousemove 1775 60 click 3
sleep 0.1
xdotool mousemove 1883 28 click 3
sleep 0.1
xdotool mousemove 1883 42 click 3

xdotool getmouselocation --shell | grep Y
xdotool getmouselocation --shell | grep X
