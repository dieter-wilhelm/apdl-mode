#!/bin/sh

#returns any defcustom variable from ansys-mode.el in quoted form
# for creating a list in ansys-submit-bug-report

cat ../ansys-mode.el | sed -n -e  "s/^(defcustom/ /p"| \
    sed -n -e "s/  \(\(\w*-*\)*\).*/'\1/p"
cat ../ansys-process.el | sed -n -e  "s/^(defcustom/ /p"| \
    sed -n -e "s/  \(\(\w*-*\)*\).*/'\1/p"
cat ../ansys-initialise.el | sed -n -e  "s/^(defcustom/ /p"| \
    sed -n -e "s/  \(\(\w*-*\)*\).*/'\1/p"
