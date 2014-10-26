#!/bin/sh
tmux set-buffer "$1"
printf "%s" "${1}" | tmux_send_clip.sh

