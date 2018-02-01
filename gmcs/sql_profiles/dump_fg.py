#!/usr/bin/python

import sys
import MySQLdb

db = MySQLdb.connect(host='localhost', user='ebender',
                     passwd='tr33house', db='MatrixTDB')

cursor = db.cursor()


# Main

cursor.execute('SELECT DISTINCT fg_grp_id FROM %s' % (sys.argv[1]))
ids = []
for id in cursor.fetchall():
    ids.append(id[0])
for i in ids:
    cursor.execute('SELECT fg_feat,fg_value FROM %s WHERE fg_grp_id = %s' %
                   (sys.argv[1], i))
    res = cursor.fetchall()
    print str(i) + ', ',
    for r in res:
        print r[0] + '=' + r[1] + ',',
    print
