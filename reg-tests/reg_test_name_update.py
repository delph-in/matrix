#!/bin/usr/env

import sys
import os
import subprocess

dir_name=sys.argv[1]

def change_name(name):
    new_name=name.replace('/','')
    new_name=new_name.replace("unidir-possessor","dep")
    new_name=new_name.replace("unidir-possessum","head")
    new_name=new_name.replace("bidirec","free-wo")
    new_name=new_name.replace("mutual","mut")
    new_name=new_name.replace("affix","aff")
    new_name=new_name.replace("non-affix","!aff")
    new_name=new_name.replace("head-final","hf")
    new_name=new_name.replace("head-initial","hi")
    new_name=new_name.replace("possessor","dep")
    new_name=new_name.replace("possessum","head")
    new_name=new_name.replace("-right-side-marker","")
    new_name=new_name.replace("-left-side-marker","")
    return new_name
new_name=change_name(dir_name)
subprocess.call("cp -r "+dir_name+" renamed/"+new_name,shell=True)
subprocess.call("rm -r renamed/"+new_name+"/"+dir_name,shell=True) #+" renamed/"+new_name+"/"+new_name,shell=True)


#Edit choices file:
choices_file=os.path.join('renamed',new_name,'choices3')
new_lines=[]
with open(choices_file) as f:
    for line in f.readlines():
        if (not "iso-code=" in line) and (not "marker-order" in line):
            if "language=" in line:
                line=change_name(line)
            new_lines.append(line)
f.close()
#new_choices=os.path.join('renamed',new_name,'choices4')
f=open(choices_file,'w')
for line in new_lines:
    f.write(line)
f.close()
subprocess.call("python ../gmcs/customize.py "+choices_file,shell=True)
#subprocess.call("rm -r renamed/*/*~",shell=True)


            

