#!/usr/bin/python
# -*- coding: UTF-8 -*-

import re

import fileinput

def main():
    
    ifile = open('empdb_biz_room.erl.1')
    ofile = open('empdb_biz_room.erl', 'w')
    
    istring = ifile.read()
    
    
    #for line in fileinput.input('empdb_biz_room.erl.1'):
    #    istring = line #"lists:member(photo.path, Fields)"
    ofile.write(re.sub(r"((\s*|[']|[{][(])([a-z_-]+[.][a-z_-]+)([']|\s*))", r"'\3'", istring, re.U | re.M ))
       
    


if (__name__ == '__main__'):
    main()
