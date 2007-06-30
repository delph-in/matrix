import os
import re
import sys

lgs = ['eng', 'epo', 'fas', 'fin', 'hau', 'heb', 'hye', 'isl', 'ita', 'zul']

sum = open(sys.argv[1],'r')
out = open('table.tex','w')

out.write("\documentclass{article}\n")
out.write("\\begin{document}\n\n")

head = sum.readline() #Through out first line

out.write("\\begin{center}\n\n" + head + "\end{center}\n\n\smallskip\n\n")

src = ''
tgt = ''
parsed = 0
results = []

for line in sum.readlines():

    if re.search(r'fan',line):
        pair = line.split('.')[0]
        [src,tgt] = pair.split('2')

    else:
        wc = line.split()[0]
        success = 17 - int(wc)
        results.append((src,tgt,str(success)))

out.write("\\begin{tabular}{")
for lg in lgs:
    out.write("|r")
out.write("|r|}\n\\hline\n")

for lg in lgs:
    out.write("&" + lg)
out.write("\\\\\\hline\n")

for src in lgs:
    out.write(src)
    for tgt in lgs:
        for res in results:
            if res[0] == src and res[1] == tgt:
                out.write("&" + res[2])
                break
        else:
            out.write("&-")
    else:
        out.write("\\\\\\hline\n")

out.write("\end{tabular}\n\n")
out.write("\end{document}\n")
        
sum.close()
out.close()

