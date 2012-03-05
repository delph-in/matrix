# Read in input files for each pair of MMT languages and create
# bitext files for that pair.  Assumes that all MMT languages have
# an input file with exactly 17 sentences, and with the items
# in the corresponding order.

import os
import re
import sys

lgs = ['eng', 'epo', 'fas', 'fin', 'hau', 'heb', 'hye', 'isl', 'ita', 'zul']
dir = '/home/ebender/logon/uw/mmt/test_sentences/'

for src in lgs:
    for tgt in lgs:
        if src != tgt:

            src_input = open(dir+src+'.txt','r')
            tgt_input = open(dir+tgt+'.txt','r')
            bitext = open(dir+src+'2'+tgt+'.txt','w')

            src_item = src_input.readline()
            while src_item:
                tgt_item = tgt_input.readline()
                if not re.search(r'^\s*$',src_item):
                    bitext.write(src_item)
                    bitext.write(tgt_item)
                    bitext.write("\n")
                src_item = src_input.readline()

            src_input.close()
            tgt_input.close()
            bitext.close()
            
