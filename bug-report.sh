#!/bin/sh

#returns any defcustom variable from ansys-modeXXX.el in quoted form

cat ansys-mode100.el | sed -n -e  "s/^(defcustom/ /p"| \
    sed -n -e "s/  \(\(\w*-*\)*\).*/'\1/p"