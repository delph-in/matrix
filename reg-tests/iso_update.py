#!/bin/usr/env

import sys
import os

dir_name=sys.argv[1]
dir_name=dir_name.replace('/','')


choices_file=os.path.join(dir_name,'choices2')


new_lines=[]
possessor_affix=False
possessum_affix=False
with open(choices_file) as f:
    for line in f:
        if 'possessor-type=affix' in line:
            possessor_affix=True
        if 'possessum-type=affix' in line:
            possessum_affix=True
        if 'language=' in line:
            new_line='language='+dir_name+'\n'
            new_lines.append(new_line)
        elif 'iso-code=' in line:
            new_line='iso-code='+dir_name+'\n'
            new_lines.append(new_line)
        # Only works for single strat choices files
        elif 'possessor-agr=' in line:
            if possessor_affix :
                line=line.replace('possessor-agr','possessor-affix-agr')
            new_lines.append(line)
        elif 'possessum-agr=' in line:
            if possessum_affix :
                line=line.replace('possessum-agr','possessum-affix-agr')
            new_lines.append(line)
        else:
            new_lines.append(line)

new_choices_file=os.path.join(dir_name,'choices3')
f_out = open(new_choices_file,'w') 
for line in new_lines:
    f_out.write(line)
f_out.close() 
