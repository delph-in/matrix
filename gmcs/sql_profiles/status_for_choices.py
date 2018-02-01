#!/usr/bin/python

import sys
import MySQLdb

db = MySQLdb.connect(host='localhost', user='ebender',
                     passwd='tr33house', db='MatrixTDB')

cursor = db.cursor()

def sql_count(table):
    cursor.execute('SELECT COUNT(*) FROM ' + table)
    return cursor.fetchall()[0][0]


# Main

print '%20s %8s %8s' % ('', 'old', 'new')
for t in ('orig_source_profile',
          'lt',
          'feat_grp',
          'lt_feat_grp',
          #          'filter',
          #          'fltr_mrs',
          'fltr_feat_grp'):
    old = sql_count(t + '_071907')
    new = sql_count(t)
    if old == new:
        check = 'X'
    else:
        check = ''
    print '%20s %8d %8d  %s' % (t, old, new, check)
