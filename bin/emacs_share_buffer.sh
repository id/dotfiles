#!/bin/sh
tmux set-buffer "$1"
tmux_send_clip.sh <<<"$1"

