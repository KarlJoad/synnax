#!/usr/bin/env -S guix shell xrandr -- bash

xrandr --output DVI-D-1 --mode 1280x1024
xrandr --output DVI-D-1 --mode 1600x900
xrandr --output HDMI-1 --left-of DVI-D-1
