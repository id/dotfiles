#!/bin/bash

clip=$(cat)
err=

hosts=( )
osx_hosts=( )
ssh="ssh -o ConnectTimeout=1 -o ConnectionAttempts=1"

for h in ${hosts[@]}; do
    err=$( printf "%s" "${clip}" | ${ssh} ${h} "tmux load-buffer -" 2>&1 )
    [ $? -ne 0 ] && tmux display-message "${err}"
done

for h in ${osx_hosts[@]}; do
    err=$( printf "%s" "${clip}" | ${ssh} ${h} pbcopy 2>&1 )
    [ $? -ne 0 ] && tmux display-message "${err}"
done
