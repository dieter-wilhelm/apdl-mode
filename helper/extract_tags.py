#! /usr/bin/env python
# #! /bin/env pyhton

#  Copyright (C) 2006 - 2020  H. Dieter Wilhelm, GPL V3

# Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
# Maintainer: H. Dieter Wilhelm
# Version: 20.2.0
# Keywords: Languages, Convenience, ANSYS
# 
# ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# This code is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 3, or (at your
# option) any later version.
# 
# This lisp script is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# 
# Permission is granted to distribute copies of this lisp script
# provided the copyright notice and this permission are preserved in
# all copies.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, you can either send email to this
# program's maintainer or write to: The Free Software Foundation,
# Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.
# ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#create the ansys keywordlist from the xml help files
# V14.5 introduced links in the refpurpose classes

import os
import glob
import re

version = "201"                 # ansys version
f_list = glob.glob("Hlp_C_CmdTOC.html")
out = "apdl_keywords.txt"
o = open(out,'w+')              # w+: write anew
o.write("# Command list for ANSYS V" + version + "\n")

# ref is the entry class for commands in a definition list
ref = '<dt><span class="refentrytitle">'
# end of definition
ref2= '</span></dt>'

cmd = 'class="command"><strong>'
cl = len( cmd)
prp = 'class="refpurpose">'
pl = len( prp)

for f in f_list:
    print "reading", f
    inf = open( f)              # default: read only mode
    o.write("# from " + f + "\n")
    s = inf.read()
    pos2 = 0
    n = 0
    while True:
        pos = s.find( ref, pos2)
        if pos < 0:
            break
        # es = complete entry string of a command definition
        pos2 = s.find( ref2, pos)
        es =  s[ pos:pos2]
        # finding command position of es
        pos3 = es.find( cmd, 0)
        pos4 = es.find( '<',pos3+cl)
        cs = es[ pos3+cl:pos4]      # command keyword string
        # finding the documentation string
        pos3 = es.find( prp, pos4)
        ps = es[ pos3+pl:len(es)].replace("\n"," ") # purpose string
        ps = re.sub('<.*?>','',ps)
        print ps
        o.write(cs + ps + "\n")
        n = n + 1
    inf.close()
o.close()
print "We got 1491 entries in the HlP_C_CmdTOC.html in V161."
print "We have ( for V",version,")", n, "entries in the HlP_C_CmdTOC.html."
print  "wrote " + out + ", that's it!"
