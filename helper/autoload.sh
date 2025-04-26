#!/bin/sh

# return a list of Emacs autoload cookies

# cat ../apdl-mode.el | sed -n -e  "s/;;;###autoload\n//p"

cat ../apdl-mode.el | sed -n '/;;;###autoload$/,/.*/p' | sed /autoload/d
cat ../apdl-process.el | sed -n '/;;;###autoload$/,/.*/p' | sed /autoload/d
