#!/bin/sh

#returns any defcustom variable from apdl-mode.el in quoted form
# for creating a list in apdl-submit-bug-report

cat ../apdl-mode.el | sed -n -e  "s/^(defcustom/ /p"| \
    sed -n -e "s/  \(\(\w*-*\)*\).*/'\1/p"
cat ../apdl-process.el | sed -n -e  "s/^(defcustom/ /p"| \
    sed -n -e "s/  \(\(\w*-*\)*\).*/'\1/p"
cat ../apdl-initialise.el | sed -n -e  "s/^(defcustom/ /p"| \
    sed -n -e "s/  \(\(\w*-*\)*\).*/'\1/p"
cat ../apdl-wb-template.el | sed -n -e  "s/^(defcustom/ /p"| \
    sed -n -e "s/  \(\(\w*-*\)*\).*/'\1/p"
