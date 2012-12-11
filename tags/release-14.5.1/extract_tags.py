#!/bin/env python

#create the ansys keywordlist from the xml help files

import os
import glob
#import sys

version = "Ansys V145"
#r_dir = "/appl"
#a_dir = "/ansys_inc/v140/commonfiles/help/en-us/help/ans_cmd"
#loc = r_dir + a_dir
#os.chdir(loc)

# CmdToc.html repesents (V13, V14) the collection of all commands
f_list = glob.glob("Hlp_C_CmdTOC.html")

#f_list = glob.glob("Hlp_*_TOC.html")
#f_list.append("Hlp_conn_cmds.html")

out = "ansys_keywords.txt"
o = open(out,'w+')              # w+: write anew
o.write("# created by a script for " + version + "\n")

cmd = 'class="command"><strong>'
cl = len( cmd)
prp = 'class="refpurpose">'
pl = len( prp)

for f in f_list:
    print "reading", f
    o.write("# " + f + "\n")
    inf = open(f)                  # default: read only mode
    s = inf.read()
    pos2 = 0
    while True:
        pos = s.find( cmd, pos2)
        if pos < 0:
            break
        pos2 = s.find( '<',pos + cl)
        cs =  s[ pos+cl:pos2]
        pos = s.find( prp, pos2)
        pos2 = s.find('<',pos)
        ps = s[ pos+pl:pos2].replace("\n"," ")
        o.write(cs + ps + "\n")
    inf.close()
o.close()
print  "that's it"
