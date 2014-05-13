#!/bin/sh
tmux set-buffer "$1"
echo -n "$1" | tmux_send_clip.sh

