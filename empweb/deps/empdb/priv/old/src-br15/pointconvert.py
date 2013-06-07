#!/usr/bin/python
# -*- coding: UTF-8 -*-

import re

import fileinput
X = [
"empdb_daowp_community.erl",
"empdb_daowp_event.erl",
"empdb_daowp_notice.erl",
"empdb_daowp_room.erl"
]

def main():
    
    
    for f in X:
        ifile = open('daowp.1/'+f)
        ofile = open('daowp/'+f, 'w')
        
        istring = ifile.read()
        
        
        #for line in fileinput.input('empdb_biz_room.erl.1'):
        #    istring = line #"lists:member(photo.path, Fields)"
        ofile.write(re.sub(r"((\s*|[']|[{][(])([a-z_-]+[.][a-z_-]+)([']|\s*))", r"'\3'", istring, re.U | re.M ).replace("""-include("'empdb.hrl'").""", """-include("empdb.hrl")."""))
        
        
        
        
        


if (__name__ == '__main__'):
    main()
