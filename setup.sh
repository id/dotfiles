#!/bin/sh

[ -d ~/bin ] || mkdir ~/bin

for f in .* .emacs.d/* bin/*; do
    if [ -f ${f} ]; then
        printf %s "Merging ${f} with ~/${f}..."
        merge ~/${f} ${f} ~/${f}
        echo "done"
    fi
done

