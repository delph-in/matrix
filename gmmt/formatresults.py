import os
import re
import sys

def read_summary(file):

    results = []

    sum = open(file,'r')

    foo = sum.readline() # throw out first line
    
    for line in sum.readlines():

        if re.search(r'fan',line):
            pair = line.split('.')[0]
            [src,tgt] = pair.split('2')

        else:
            wc = line.split()[0]
            success = 17 - int(wc)
            results.append((src,tgt,str(success)))

    sum.close()
    return results


def write_table_headings():

    out.write("\\begin{tabular}{")
    for lg in lgs:
        out.write("|r")
    out.write("|r|}\n\\hline\n")

    for lg in lgs:
        out.write("&" + lg)
    out.write("\\\\\\hline\n")



lgs = ['eng', 'epo', 'fas', 'fin', 'hau', 'heb', 'hye', 'isl', 'ita', 'zul']

new_results = read_summary(sys.argv[1])
old_results = read_summary(sys.argv[2])

out = open('table.tex','w')

out.write("\documentclass{article}\n")
out.write("\\begin{document}\n\n")

out.write("\\begin{center}\n\n" + sys.argv[1] + "\end{center}\n\n\smallskip\n\n")

src = ''
tgt = ''
parsed = 0

write_table_headings()

for src in lgs:
    out.write(src)
    for tgt in lgs:
        for res in new_results:
            if res[0] == src and res[1] == tgt:
                out.write("&" + res[2])
                break
        else:
            out.write("&-")
    else:
        out.write("\\\\\\hline\n")

out.write("\end{tabular}\n\n")

out.write("\smallskip\n\n\\begin{center}\n\n Comparison to " + sys.argv[2] + "\end{center}\n\n\smallskip\n\n")


write_table_headings()

for src in lgs:
    out.write(src)
    for tgt in lgs:
        for res in new_results:
            if res[0] == src and res[1] == tgt:
                for comp in old_results:
                    if comp[0] == src and comp[1] == tgt:
                        out.write("&" + str(int(res[2])-int(comp[2])))
                        break
                break
        else:
            out.write("&-")
    else:
        out.write("\\\\\\hline\n")

out.write("\end{tabular}\n\n")
out.write("\end{document}\n")
        
out.close()

